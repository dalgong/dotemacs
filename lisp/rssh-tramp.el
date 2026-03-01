;;; rssh-tramp.el --- TRAMP method for rssh with RPC -*- lexical-binding: t; -*-

;; Author: rssh
;; Keywords: comm, processes
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Provides a TRAMP method "rssh" that uses the rssh SSH multiplexer for
;; remote file access.  File operations are performed via JSON-RPC through
;; `rssh --rpc host`, giving single-round-trip performance for stat, read,
;; write, directory listing, etc.  Shell access for async processes uses
;; `rssh --pipe host`.
;;
;; Usage:
;;   C-x C-f /rssh:hostname:/path/to/file
;;   C-x C-f /rssh:user@hostname:/path/to/file
;;
;; Setup:
;;   1. Ensure `rssh` (the wrapper script) is on your PATH.
;;   2. Add to your init file:
;;
;;        (require 'rssh-tramp)
;;
;; For best results, pre-establish the connection from a terminal:
;;   $ rssh connect myhost
;; Then all TRAMP operations reuse that single SSH connection.

;;; Code:

(require 'tramp)
(require 'tramp-sh)
(require 'json)

(defgroup rssh-tramp nil
  "TRAMP method using rssh multiplexer with RPC."
  :group 'tramp
  :prefix "rssh-tramp-")

(defcustom rssh-tramp-rssh-program "rssh"
  "Path to the rssh program."
  :type 'string
  :group 'rssh-tramp)

(defconst rssh-tramp-method "rssh"
  "TRAMP method for rssh connections.")

;; Register the TRAMP shell method (for async processes / direct-async)
(setq tramp-methods
      (assoc-delete-all rssh-tramp-method tramp-methods))

(add-to-list 'tramp-methods
             `(,rssh-tramp-method
               (tramp-login-program        ,rssh-tramp-rssh-program)
               (tramp-login-args            (("-l" "%u") ("--pipe") ("%h")))
               (tramp-direct-async          ("-t"))
               (tramp-remote-shell          "/bin/bash")
               (tramp-remote-shell-login    ("-l"))
               (tramp-remote-shell-args     ("-c"))
               (tramp-connection-timeout    60)))

;; Enable direct async processes
(connection-local-set-profile-variables
 'rssh-tramp-direct-async-profile
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 `(:application tramp :protocol ,rssh-tramp-method)
 'rssh-tramp-direct-async-profile)

(add-to-list 'tramp-connection-properties
             (list (concat "\\`/" rssh-tramp-method ":")
                   "direct-async-process" t))

;; Register completion function
(tramp-set-completion-function
 rssh-tramp-method tramp-completion-function-alist-ssh)

;; ============================================================================
;; RPC Connection Management
;; ============================================================================

(defvar rssh-tramp--rpc-connections (make-hash-table :test 'equal)
  "Hash table mapping host keys to RPC process info.
Key is (host user), value is plist (:process :buffer :request-id).")

(defvar rssh-tramp--rpc-pending (make-hash-table :test 'equal)
  "Hash table mapping (buffer . request-id) to response strings.")

(defun rssh-tramp--rpc-key (vec)
  "Generate RPC connection key for VEC."
  (list (tramp-file-name-host vec)
        (or (tramp-file-name-user vec) "")))

(defun rssh-tramp--get-rpc (vec)
  "Get or create RPC connection for VEC."
  (let* ((key (rssh-tramp--rpc-key vec))
         (conn (gethash key rssh-tramp--rpc-connections)))
    (if (and conn
             (process-live-p (plist-get conn :process))
             (buffer-live-p (plist-get conn :buffer)))
        conn
      ;; Need new connection
      (when conn
        (let ((proc (plist-get conn :process)))
          (when (process-live-p proc) (delete-process proc))))
      (rssh-tramp--start-rpc vec key))))

(defun rssh-tramp--start-rpc (vec key)
  "Start an RPC connection for VEC with KEY."
  ;; Clear stale system-info cache for this host
  (remhash key rssh-tramp--system-info-cache)
  (let* ((host (tramp-file-name-host vec))
         (user (tramp-file-name-user vec))
         (target (if user (format "%s@%s" user host) host))
         (buf (generate-new-buffer (format " *rssh-rpc %s*" target)))
         (proc (start-process
                (format "rssh-rpc-%s" target) buf
                rssh-tramp-rssh-program "--rpc" target)))
    (set-process-query-on-exit-flag proc nil)
    (set-process-coding-system proc 'utf-8 'utf-8)
    (set-process-filter proc #'rssh-tramp--rpc-filter)
    (let ((conn (list :process proc :buffer buf :request-id 0)))
      (puthash key conn rssh-tramp--rpc-connections)
      conn)))

(defun rssh-tramp--rpc-filter (proc output)
  "Process filter for RPC connections.
Parses complete JSON lines and stores them keyed by response id."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (goto-char (point-max))
        (insert output)
        ;; Process complete lines
        (goto-char (point-min))
        (while (search-forward "\n" nil t)
          (let* ((line (buffer-substring (point-min) (1- (point)))))
            (delete-region (point-min) (point))
            (goto-char (point-min))
            ;; Parse to extract id, store keyed by (buffer . id)
            (condition-case nil
                (let* ((parsed (json-read-from-string line))
                       (id (cdr (assoc 'id parsed))))
                  (when id
                    (puthash (cons buf id) line rssh-tramp--rpc-pending)))
              (error nil))))))))

(defcustom rssh-tramp-rpc-timeout 30
  "Timeout in seconds for RPC calls."
  :type 'integer
  :group 'rssh-tramp)

(defun rssh-tramp--rpc-call (vec method params)
  "Call METHOD with PARAMS via RPC on VEC. Returns parsed JSON result."
  (let* ((conn (rssh-tramp--get-rpc vec))
         (proc (plist-get conn :process))
         (buf (plist-get conn :buffer))
         (id (1+ (plist-get conn :request-id)))
         (request (json-encode `((method . ,method) (params . ,params))))
         (key (cons buf id)))
    ;; Update the request-id in the connection plist
    (plist-put conn :request-id id)
    ;; Send request
    (process-send-string proc (concat request "\n"))
    ;; Wait for response with matching id
    (let ((deadline (+ (float-time) rssh-tramp-rpc-timeout))
          response)
      (while (and (not response)
                  (< (float-time) deadline)
                  (process-live-p proc))
        (accept-process-output proc 0.1)
        (setq response (gethash key rssh-tramp--rpc-pending)))
      (when response
        (remhash key rssh-tramp--rpc-pending))
      (unless response
        (error "Timeout waiting for RPC response from %s (method=%s)"
               (tramp-file-name-host vec) method))
      (let ((parsed (json-read-from-string response)))
        (if (assoc 'error parsed)
            (let ((msg (cdr (assoc 'message (cdr (assoc 'error parsed))))))
              (error "RPC error: %s" msg))
          (cdr (assoc 'ok parsed)))))))

(defun rssh-tramp--rpc-call-batch (vec requests)
  "Call multiple REQUESTS in a single batch RPC.
REQUESTS is a list of (METHOD . PARAMS) cons cells.
Returns a list of results."
  (let ((result (rssh-tramp--rpc-call
                 vec "batch"
                 `((requests . ,(vconcat
                                 (mapcar (lambda (r)
                                           `((method . ,(car r))
                                             (params . ,(cdr r))))
                                         requests)))))))
    (append (cdr (assoc 'results result)) nil)))

;; ============================================================================
;; File name handler
;; ============================================================================

(defsubst rssh-tramp-file-name-p (vec-or-filename)
  "Check if VEC-OR-FILENAME is handled by rssh-tramp."
  (when-let* ((vec (tramp-ensure-dissected-file-name vec-or-filename)))
    (string= (tramp-file-name-method vec) rssh-tramp-method)))

(defconst rssh-tramp-file-name-handler-alist
  '((access-file . rssh-tramp-handle-access-file)
    (add-name-to-file . tramp-handle-add-name-to-file)
    (byte-compiler-base-file-name . ignore)
    (copy-directory . tramp-handle-copy-directory)
    (copy-file . rssh-tramp-handle-copy-file)
    (delete-directory . rssh-tramp-handle-delete-directory)
    (delete-file . rssh-tramp-handle-delete-file)
    (diff-latest-backup-file . ignore)
    (directory-file-name . tramp-handle-directory-file-name)
    (directory-files . rssh-tramp-handle-directory-files)
    (directory-files-and-attributes . rssh-tramp-handle-directory-files-and-attributes)
    (dired-compress-file . ignore)
    (dired-uncache . tramp-handle-dired-uncache)
    (exec-path . tramp-sh-handle-exec-path)
    (expand-file-name . rssh-tramp-handle-expand-file-name)
    (file-accessible-directory-p . tramp-handle-file-accessible-directory-p)
    (file-acl . ignore)
    (file-attributes . rssh-tramp-handle-file-attributes)
    (file-directory-p . tramp-handle-file-directory-p)
    (file-equal-p . tramp-handle-file-equal-p)
    (file-executable-p . rssh-tramp-handle-file-executable-p)
    (file-exists-p . rssh-tramp-handle-file-exists-p)
    (file-in-directory-p . tramp-handle-file-in-directory-p)
    (file-local-copy . rssh-tramp-handle-file-local-copy)
    (file-locked-p . tramp-handle-file-locked-p)
    (file-modes . tramp-handle-file-modes)
    (file-name-all-completions . rssh-tramp-handle-file-name-all-completions)
    (file-name-as-directory . tramp-handle-file-name-as-directory)
    (file-name-case-insensitive-p . tramp-handle-file-name-case-insensitive-p)
    (file-name-completion . tramp-handle-file-name-completion)
    (file-name-directory . tramp-handle-file-name-directory)
    (file-name-nondirectory . tramp-handle-file-name-nondirectory)
    (file-newer-than-file-p . tramp-handle-file-newer-than-file-p)
    (file-notify-add-watch . ignore)
    (file-notify-rm-watch . ignore)
    (file-notify-valid-p . ignore)
    (file-ownership-preserved-p . ignore)
    (file-readable-p . tramp-handle-file-readable-p)
    (file-regular-p . tramp-handle-file-regular-p)
    (file-remote-p . tramp-handle-file-remote-p)
    (file-selinux-context . tramp-handle-file-selinux-context)
    (file-symlink-p . tramp-handle-file-symlink-p)
    (file-system-info . ignore)
    (file-truename . rssh-tramp-handle-file-truename)
    (file-writable-p . tramp-handle-file-writable-p)
    (find-backup-file-name . tramp-handle-find-backup-file-name)
    (insert-directory . tramp-handle-insert-directory)
    (insert-file-contents . rssh-tramp-handle-insert-file-contents)
    (load . tramp-handle-load)
    (lock-file . tramp-handle-lock-file)
    (make-auto-save-file-name . tramp-handle-make-auto-save-file-name)
    (make-directory . rssh-tramp-handle-make-directory)
    (make-lock-file-name . tramp-handle-make-lock-file-name)
    (make-nearby-temp-file . tramp-handle-make-nearby-temp-file)
    (make-process . tramp-handle-make-process)
    (make-symbolic-link . rssh-tramp-handle-make-symbolic-link)
    (process-file . rssh-tramp-handle-process-file)
    (rename-file . rssh-tramp-handle-rename-file)
    (set-file-acl . ignore)
    (set-file-modes . rssh-tramp-handle-set-file-modes)
    (set-file-selinux-context . ignore)
    (set-file-times . tramp-handle-set-file-times)
    (set-visited-file-modtime . tramp-handle-set-visited-file-modtime)
    (shell-command . tramp-handle-shell-command)
    (start-file-process . tramp-handle-start-file-process)
    (substitute-in-file-name . tramp-handle-substitute-in-file-name)
    (temporary-file-directory . tramp-handle-temporary-file-directory)
    (tramp-get-home-directory . rssh-tramp-handle-get-home-directory)
    (tramp-get-remote-gid . rssh-tramp-handle-get-remote-gid)
    (tramp-get-remote-groups . ignore)
    (tramp-get-remote-uid . rssh-tramp-handle-get-remote-uid)
    (tramp-set-file-uid-gid . ignore)
    (unhandled-file-name-directory . ignore)
    (unlock-file . tramp-handle-unlock-file)
    (vc-registered . ignore)
    (verify-visited-file-modtime . tramp-handle-verify-visited-file-modtime)
    (write-region . rssh-tramp-handle-write-region))
  "Alist of file name handler functions for rssh-tramp.")

;;;###autoload
(defun rssh-tramp-file-name-handler (operation &rest args)
  "Invoke rssh-tramp file name handler for OPERATION with ARGS."
  (let ((handler (assoc operation rssh-tramp-file-name-handler-alist)))
    (if handler
        (save-match-data (apply (cdr handler) args))
      (tramp-run-real-handler operation args))))

;; Register as foreign handler
(add-to-list 'tramp-foreign-file-name-handler-alist
             '(rssh-tramp-file-name-p . rssh-tramp-file-name-handler))

;; ============================================================================
;; Handler implementations
;; ============================================================================

(defvar rssh-tramp--system-info-cache (make-hash-table :test 'equal)
  "Cache for system.info results per host.")

(defun rssh-tramp--system-info (vec)
  "Get cached system info for VEC."
  (let ((key (rssh-tramp--rpc-key vec)))
    (or (gethash key rssh-tramp--system-info-cache)
        (let ((info (rssh-tramp--rpc-call vec "system.info" nil)))
          (puthash key info rssh-tramp--system-info-cache)
          info))))

(defun rssh-tramp-handle-get-home-directory (vec &optional _user)
  "Return remote home directory for VEC."
  (with-tramp-connection-property vec "home-directory"
    (let ((info (rssh-tramp--system-info vec)))
      (cdr (assoc 'home info)))))

(defun rssh-tramp-handle-get-remote-uid (vec id-format)
  "Return remote uid for VEC in ID-FORMAT."
  (let ((info (rssh-tramp--system-info vec)))
    (if (eq id-format 'integer)
        (cdr (assoc 'uid info))
      (number-to-string (cdr (assoc 'uid info))))))

(defun rssh-tramp-handle-get-remote-gid (vec id-format)
  "Return remote gid for VEC in ID-FORMAT."
  (let ((info (rssh-tramp--system-info vec)))
    (if (eq id-format 'integer)
        (cdr (assoc 'gid info))
      (number-to-string (cdr (assoc 'gid info))))))

(defun rssh-tramp-handle-expand-file-name (name &optional dir)
  "Like `expand-file-name' for rssh TRAMP files.
When LOCALNAME is empty or non-absolute, treat it as relative to
the home directory (prepend \"~/\"), matching tramp-sh behavior."
  (setq dir (or dir default-directory "/"))
  (when (string-empty-p name) (setq name "."))
  (unless (file-name-absolute-p name)
    (setq name (file-name-concat dir name)))
  (if (not (tramp-tramp-file-p name))
      (tramp-run-real-handler #'expand-file-name (list name))
    (with-parsed-tramp-file-name name nil
      ;; Key difference from tramp-handle-expand-file-name:
      ;; non-absolute localname gets "~/" prefix, not just "/".
      (unless (tramp-run-real-handler #'file-name-absolute-p (list localname))
        (setq localname (concat "~/" localname)))
      ;; Tilde expansion
      (when (string-prefix-p "~" (file-name-unquote localname))
        (setq localname (file-name-unquote localname)))
      (when (string-match
             (rx bos "~" (group (* (not "/"))) (group (* nonl)) eos) localname)
        (let ((uname (match-string 1 localname))
              (fname (match-string 2 localname))
              hname)
          (when (tramp-string-empty-or-nil-p uname)
            (setq uname user))
          (when (setq hname (tramp-get-home-directory v uname))
            (setq localname (concat hname fname)))))
      (when (and (not (bound-and-true-p tramp-tolerate-tilde))
                 (string-prefix-p "~" localname))
        (tramp-error v 'file-error "Cannot expand tilde in file `%s'" name))
      (when (string-match-p (rx bos "/" (** 1 2 ".") eos) localname)
        (setq localname "/"))
      (let ((default-directory tramp-compat-temporary-file-directory))
        (tramp-make-tramp-file-name
         v (tramp-drop-volume-letter
            (if (string-prefix-p "~" localname)
                localname
              (tramp-run-real-handler
               #'expand-file-name (list localname)))))))))

(defun rssh-tramp-handle-access-file (filename string)
  "Like `access-file' for rssh TRAMP files."
  (unless (file-readable-p filename)
    (tramp-error
     (tramp-dissect-file-name filename) tramp-file-missing
     "%s: No such file or directory" string filename)))

(defun rssh-tramp--mode-string (mode type)
  "Convert numeric MODE to a string like \"drwxr-xr-x\".
TYPE is the file type string."
  (let ((tc (pcase type
              ("directory" ?d) ("symlink" ?l) ("chardevice" ?c)
              ("blockdevice" ?b) ("fifo" ?p) ("socket" ?s) (_ ?-))))
    (format "%c%c%c%c%c%c%c%c%c%c"
            tc
            (if (> (logand mode #o400) 0) ?r ?-)
            (if (> (logand mode #o200) 0) ?w ?-)
            (if (> (logand mode #o4000) 0)
                (if (> (logand mode #o100) 0) ?s ?S)
              (if (> (logand mode #o100) 0) ?x ?-))
            (if (> (logand mode #o040) 0) ?r ?-)
            (if (> (logand mode #o020) 0) ?w ?-)
            (if (> (logand mode #o2000) 0)
                (if (> (logand mode #o010) 0) ?s ?S)
              (if (> (logand mode #o010) 0) ?x ?-))
            (if (> (logand mode #o004) 0) ?r ?-)
            (if (> (logand mode #o002) 0) ?w ?-)
            (if (> (logand mode #o1000) 0)
                (if (> (logand mode #o001) 0) ?t ?T)
              (if (> (logand mode #o001) 0) ?x ?-)))))

(defun rssh-tramp--convert-stat (stat id-format)
  "Convert STAT alist to Emacs file-attributes format."
  (let* ((type-str (cdr (assoc 'type stat)))
         (type (pcase type-str
                 ("file" nil)
                 ("directory" t)
                 ("symlink" (cdr (assoc 'link_target stat)))
                 (_ nil)))
         (mode (cdr (assoc 'mode stat)))
         (uid (cdr (assoc 'uid stat)))
         (gid (cdr (assoc 'gid stat))))
    (list type
          (cdr (assoc 'nlinks stat))
          (if (eq id-format 'string)
              (or (cdr (assoc 'uname stat)) (number-to-string uid))
            uid)
          (if (eq id-format 'string)
              (or (cdr (assoc 'gname stat)) (number-to-string gid))
            gid)
          (seconds-to-time (cdr (assoc 'atime stat)))
          (seconds-to-time (cdr (assoc 'mtime stat)))
          (seconds-to-time (cdr (assoc 'ctime stat)))
          (cdr (assoc 'size stat))
          (rssh-tramp--mode-string mode type-str)
          nil
          (cdr (assoc 'inode stat))
          (cdr (assoc 'dev stat)))))

(defun rssh-tramp-handle-file-attributes (filename &optional id-format)
  "Like `file-attributes' for rssh TRAMP files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property
        v localname (format "file-attributes-%s" id-format)
      (condition-case nil
          (let ((stat (rssh-tramp--rpc-call
                       v "stat" `((path . ,localname)))))
            (rssh-tramp--convert-stat stat id-format))
        (error nil)))))

(defun rssh-tramp-handle-file-exists-p (filename)
  "Like `file-exists-p' for rssh TRAMP files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-exists-p"
      (condition-case nil
          (let ((r (rssh-tramp--rpc-call v "exists" `((path . ,localname)))))
            (eq (cdr (assoc 'exists r)) t))
        (error nil)))))

(defun rssh-tramp-handle-file-executable-p (filename)
  "Like `file-executable-p' for rssh TRAMP files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-executable-p"
      (when-let* ((attrs (file-attributes filename 'integer)))
        (let ((mode-string (file-attribute-modes attrs))
              (remote-uid (tramp-get-remote-uid v 'integer)))
          (or (memq (aref mode-string 9) '(?x ?t))
              (and (memq (aref mode-string 3) '(?x ?s))
                   (or (equal remote-uid tramp-root-id-integer)
                       (equal remote-uid (file-attribute-user-id attrs))))
              (and (memq (aref mode-string 6) '(?x ?s))
                   (equal (tramp-get-remote-gid v 'integer)
                          (file-attribute-group-id attrs)))))))))

(defun rssh-tramp-handle-file-truename (filename)
  "Like `file-truename' for rssh TRAMP files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (with-tramp-file-property v localname "file-truename"
      (condition-case nil
          (let* ((r (rssh-tramp--rpc-call
                     v "truename" `((path . ,localname))))
                 (path (cdr (assoc 'path r))))
            (tramp-make-tramp-file-name v (or path localname)))
        (error (tramp-make-tramp-file-name v localname))))))

(defun rssh-tramp-handle-directory-files (directory &optional full match nosort count)
  "Like `directory-files' for rssh TRAMP files."
  (with-parsed-tramp-file-name (expand-file-name directory) nil
    (let* ((result (rssh-tramp--rpc-call
                    v "dir.list" `((path . ,localname) (attrs . :json-false))))
           (files (mapcar (lambda (e) (cdr (assoc 'name e)))
                          (append result nil))))
      ;; Ensure . and .. are present
      (unless (member "." files) (push "." files))
      (unless (member ".." files) (push ".." files))
      ;; Filter by match
      (when match
        (setq files (cl-remove-if-not (lambda (f) (string-match-p match f)) files)))
      ;; Sort
      (unless nosort
        (setq files (sort files #'string<)))
      ;; Full path
      (when full
        (setq files (mapcar (lambda (f)
                              (tramp-make-tramp-file-name
                               v (expand-file-name f localname)))
                            files)))
      ;; Count
      (when count
        (setq files (seq-take files count)))
      files)))

(defun rssh-tramp-handle-directory-files-and-attributes
    (directory &optional full match nosort id-format count)
  "Like `directory-files-and-attributes' for rssh TRAMP files."
  (with-parsed-tramp-file-name (expand-file-name directory) nil
    (let* ((result (rssh-tramp--rpc-call
                    v "dir.list" `((path . ,localname) (attrs . t)
                                   (hidden . t))))
           (entries
            (mapcar (lambda (e)
                      (let* ((name (cdr (assoc 'name e)))
                             (attrs-data (cdr (assoc 'attrs e)))
                             (full-name (if full
                                            (tramp-make-tramp-file-name
                                             v (expand-file-name name localname))
                                          name)))
                        (cons full-name
                              (when attrs-data
                                (rssh-tramp--convert-stat
                                 (append `((type . ,(cdr (assoc 'type e))))
                                         attrs-data)
                                 id-format)))))
                    (append result nil))))
      (when match
        (setq entries (cl-remove-if-not
                       (lambda (e) (string-match-p match (car e))) entries)))
      (unless nosort
        (setq entries (sort entries (lambda (a b) (string< (car a) (car b))))))
      (when count
        (setq entries (seq-take entries count)))
      entries)))

(defun rssh-tramp-handle-file-name-all-completions (filename directory)
  "Like `file-name-all-completions' for rssh TRAMP files."
  (with-parsed-tramp-file-name (expand-file-name directory) nil
    (when (tramp-connectable-p v)
      (condition-case nil
          (let* ((result (rssh-tramp--rpc-call
                          v "dir.list" `((path . ,localname)
                                         (attrs . :json-false)
                                         (hidden . t))))
                 (entries (mapcar (lambda (e)
                                   (let ((name (cdr (assoc 'name e)))
                                         (type (cdr (assoc 'type e))))
                                     (if (equal type "directory")
                                         (concat name "/")
                                       name)))
                                 (append result nil))))
            (all-completions filename entries))
        (error nil)))))

(defun rssh-tramp-handle-insert-file-contents
    (filename &optional visit beg end replace)
  "Like `insert-file-contents' for rssh TRAMP files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    (let* ((params `((path . ,localname)
                     ,@(when (not (null beg)) `((offset . ,beg)))
                     ,@(when (not (null end)) `((length . ,(- end (or beg 0)))))))
           (result (rssh-tramp--rpc-call v "read" params))
           (content (base64-decode-string (cdr (assoc 'content result))))
           (size (length content)))
      (when replace (erase-buffer))
      (let ((coding (or coding-system-for-read
                        buffer-file-coding-system
                        'undecided)))
        (insert (decode-coding-string content coding)))
      (when visit
        (setq buffer-file-name filename)
        (setq buffer-read-only (not (file-writable-p filename)))
        (set-visited-file-modtime)
        (set-buffer-modified-p nil))
      (list filename size))))

(defun rssh-tramp-handle-write-region
    (start end filename &optional append visit lockname mustbenew)
  "Like `write-region' for rssh TRAMP files."
  (setq filename (expand-file-name filename))
  (with-parsed-tramp-file-name filename nil
    ;; Handle mustbenew
    (when (and mustbenew (file-exists-p filename)
               (or (eq mustbenew 'excl)
                   (not (y-or-n-p
                         (format "File %s exists; overwrite anyway?" filename)))))
      (signal 'file-already-exists (list filename)))
    (let* ((content (if (stringp start)
                        start
                      (buffer-substring-no-properties
                       (or start (point-min))
                       (or end (point-max)))))
           (coding (or coding-system-for-write
                       buffer-file-coding-system
                       'utf-8-unix))
           (encoded (encode-coding-string content coding)))
      (rssh-tramp--rpc-call v "write"
                            `((path . ,localname)
                              (content . ,(base64-encode-string encoded t))
                              (append . ,(if append t :json-false))))
      (tramp-flush-file-properties v localname)
      (when (or (eq visit t) (stringp visit))
        (setq buffer-file-name filename)
        (set-visited-file-modtime)
        (set-buffer-modified-p nil))
      (when (stringp visit)
        (message "Wrote %s" visit)))))

(defun rssh-tramp-handle-file-local-copy (filename)
  "Like `file-local-copy' for rssh TRAMP files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (let* ((result (rssh-tramp--rpc-call v "read"
                                         `((path . ,localname))))
           (content (base64-decode-string (cdr (assoc 'content result))))
           (tmpfile (tramp-make-tramp-temp-file v)))
      (with-temp-file tmpfile
        (set-buffer-multibyte nil)
        (insert content))
      tmpfile)))

(defun rssh-tramp-handle-copy-file
    (filename newname &optional ok-if-already-exists keep-time
              _preserve-uid-gid _preserve-permissions)
  "Like `copy-file' for rssh TRAMP files."
  (setq filename (expand-file-name filename)
        newname (expand-file-name newname))
  (when (and (directory-name-p newname) (file-directory-p newname))
    (setq newname (expand-file-name
                   (file-name-nondirectory filename) newname)))
  (unless ok-if-already-exists
    (when (file-exists-p newname)
      (signal 'file-already-exists (list newname))))
  (let ((src-remote (tramp-tramp-file-p filename))
        (dst-remote (tramp-tramp-file-p newname)))
    (cond
     ;; Both on same remote - server-side copy
     ((and src-remote dst-remote (tramp-equal-remote filename newname))
      (with-parsed-tramp-file-name filename v1
        (with-parsed-tramp-file-name newname v2
          (rssh-tramp--rpc-call v1 "copy"
                                `((src . ,v1-localname)
                                  (dest . ,v2-localname)
                                  (preserve . ,(if keep-time t :json-false)))))))
     ;; Remote -> local: download
     ((and src-remote (not dst-remote))
      (let ((tmpfile (file-local-copy filename)))
        (rename-file tmpfile newname ok-if-already-exists)))
     ;; Local -> remote: upload
     ((and (not src-remote) dst-remote)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally filename)
        (write-region (point-min) (point-max) newname nil 'nomessage)))
     ;; Fallback
     (t (tramp-run-real-handler
         #'copy-file
         (list filename newname ok-if-already-exists keep-time)))))
  ;; Flush caches
  (when dst-remote
    (with-parsed-tramp-file-name newname v2
      (tramp-flush-file-properties v2 v2-localname))))

(defun rssh-tramp-handle-rename-file (filename newname &optional ok-if-already-exists)
  "Like `rename-file' for rssh TRAMP files."
  (setq filename (expand-file-name filename)
        newname (expand-file-name newname))
  (when (and (directory-name-p newname) (file-directory-p newname))
    (setq newname (expand-file-name
                   (file-name-nondirectory filename) newname)))
  (let ((src-remote (tramp-tramp-file-p filename))
        (dst-remote (tramp-tramp-file-p newname)))
    (cond
     ((and src-remote dst-remote (tramp-equal-remote filename newname))
      (with-parsed-tramp-file-name filename v1
        (with-parsed-tramp-file-name newname v2
          (rssh-tramp--rpc-call v1 "rename"
                                `((src . ,v1-localname)
                                  (dest . ,v2-localname))))))
     (t
      (copy-file filename newname ok-if-already-exists t t t)
      (if (file-directory-p filename)
          (delete-directory filename 'recursive)
        (delete-file filename)))))
  (when (tramp-tramp-file-p filename)
    (with-parsed-tramp-file-name filename v1
      (tramp-flush-file-properties v1 v1-localname)))
  (when (tramp-tramp-file-p newname)
    (with-parsed-tramp-file-name newname v2
      (tramp-flush-file-properties v2 v2-localname))))

(defun rssh-tramp-handle-delete-file (filename &optional _trash)
  "Like `delete-file' for rssh TRAMP files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (rssh-tramp--rpc-call v "delete" `((path . ,localname)))
    (tramp-flush-file-properties v localname)))

(defun rssh-tramp-handle-delete-directory (directory &optional recursive _trash)
  "Like `delete-directory' for rssh TRAMP files."
  (with-parsed-tramp-file-name (expand-file-name directory) nil
    (rssh-tramp--rpc-call v "rmdir"
                          `((path . ,localname)
                            (recursive . ,(if recursive t :json-false))))
    (tramp-flush-directory-properties v localname)))

(defun rssh-tramp-handle-make-directory (dir &optional parents)
  "Like `make-directory' for rssh TRAMP files."
  (with-parsed-tramp-file-name (expand-file-name dir) nil
    (rssh-tramp--rpc-call v "mkdir"
                          `((path . ,localname)
                            (parents . ,(if parents t :json-false))))
    (tramp-flush-directory-properties v localname)))

(defun rssh-tramp-handle-make-symbolic-link (target linkname &optional ok-if-already-exists)
  "Like `make-symbolic-link' for rssh TRAMP files."
  (with-parsed-tramp-file-name (expand-file-name linkname) nil
    (when (and (not ok-if-already-exists) (file-exists-p linkname))
      (signal 'file-already-exists (list linkname)))
    (when (file-exists-p linkname) (delete-file linkname))
    (rssh-tramp--rpc-call v "symlink"
                          `((target . ,target) (link_path . ,localname)))
    (tramp-flush-file-properties v localname)))

(defun rssh-tramp-handle-set-file-modes (filename mode &optional _flag)
  "Like `set-file-modes' for rssh TRAMP files."
  (with-parsed-tramp-file-name (expand-file-name filename) nil
    (rssh-tramp--rpc-call v "chmod" `((path . ,localname) (mode . ,mode)))
    (tramp-flush-file-properties v localname)))

(defun rssh-tramp-handle-process-file
    (program &optional infile destination display &rest args)
  "Like `process-file' for rssh TRAMP files.
Runs PROGRAM via RPC `run' method."
  (with-parsed-tramp-file-name (expand-file-name default-directory) nil
    (let* ((result (rssh-tramp--rpc-call
                    v "run"
                    `((cmd . ,program)
                      (args . ,(vconcat args))
                      (cwd . ,localname))))
           (exit-code (cdr (assoc 'exit_code result)))
           (stdout-b64 (cdr (assoc 'stdout result)))
           (stderr-b64 (cdr (assoc 'stderr result)))
           (stdout (if (and stdout-b64 (> (length stdout-b64) 0))
                       (base64-decode-string stdout-b64)
                     ""))
           (stderr (if (and stderr-b64 (> (length stderr-b64) 0))
                       (base64-decode-string stderr-b64)
                     "")))
      ;; Route output
      (cond
       ((null destination) nil)
       ((eq destination t)
        (insert (decode-coding-string stdout 'undecided)))
       ((stringp destination)
        (with-temp-file destination
          (insert stdout)))
       ((bufferp destination)
        (with-current-buffer destination
          (insert (decode-coding-string stdout 'undecided))))
       ((consp destination)
        (let ((stdout-dest (car destination))
              (stderr-dest (cadr destination)))
          (when stdout-dest
            (cond
             ((eq stdout-dest t)
              (insert (decode-coding-string stdout 'undecided)))
             ((stringp stdout-dest)
              (with-temp-file stdout-dest (insert stdout)))
             ((bufferp stdout-dest)
              (with-current-buffer stdout-dest
                (insert (decode-coding-string stdout 'undecided))))))
          (when (and stderr-dest stderr)
            (cond
             ((stringp stderr-dest)
              (with-temp-file stderr-dest (insert stderr)))
             ((bufferp stderr-dest)
              (with-current-buffer stderr-dest
                (insert (decode-coding-string stderr 'undecided)))))))))
      ;; Display
      (when display (redisplay))
      ;; Flush cache if the command might modify files
      (tramp-flush-directory-properties v localname)
      exit-code)))

;; Safety net for shell-based connection recovery
(defun rssh-tramp--recover-connection (orig-fun vec)
  "Catch args-out-of-range in rssh connections and retry after cleanup."
  (condition-case err
      (funcall orig-fun vec)
    (args-out-of-range
     (if (and (tramp-file-name-p vec)
              (string= (tramp-file-name-method vec) rssh-tramp-method))
         (progn
           (tramp-cleanup-connection vec 'keep-debug 'keep-password)
           (funcall orig-fun vec))
       (signal (car err) (cdr err))))))

(advice-add 'tramp-maybe-open-connection :around
            #'rssh-tramp--recover-connection)

(provide 'rssh-tramp)
;;; rssh-tramp.el ends here
