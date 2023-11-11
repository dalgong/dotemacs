(require 'ob)

(defvar org-babel-default-header-args:compile
  '((:dir . nil) (:results . "silent"))
  "Default arguments to use when evaluating a compile source block.")

(defvar org-babel-execute:default-directory nil)
(defvar org-babel-execute:compile-dir-expander nil)

(defvar org-babel--expand-body-inside-expansion nil)
(advice-add 'org-babel--expand-body :around #'org-babel--expand-body-indicate-recursion)
(defun org-babel--expand-body-indicate-recursion (o &rest args)
  (let ((ov org-babel--expand-body-inside-expansion))
    (setq org-babel--expand-body-inside-expansion t)
    (unwind-protect
        (apply o args)
      (setq org-babel--expand-body-inside-expansion ov))))

(defun org-babel-execute:compile (body params)
  (let* ((command (org-babel-expand-body:compile body params))
         (dir (or (cdr (assq :dir params))
                  org-babel-execute:default-directory
                  (read-string "directory to run: ")))
         (default-directory (or (and org-babel-execute:compile-dir-expander
                                     (funcall org-babel-execute:compile-dir-expander dir))
                                (and (string-match "^/" dir)
                                     dir)
                                (cl-loop for d in (split-string (or (getenv "CDPATH") "") ":" t)
                                         for path = (concat d "/" dir "/")
                                         if (file-directory-p path)
                                         return path)
                                dir)))
    (if org-babel--expand-body-inside-expansion
        command
      (compile command)
      nil)))

(defun org-babel-execute:compile (body params)
  (let* ((command (org-babel-expand-body:compile body params))
         (dir (or (cdr (assq :dir params))
                  org-babel-execute:default-directory
                  (read-string "directory to run: ")))
         (default-directory (or (and org-babel-execute:compile-dir-expander
                                     (funcall org-babel-execute:compile-dir-expander dir))
                                (and (string-match "^/" dir)
                                     dir)
                                (cl-loop for d in (split-string (or (getenv "CDPATH") "") ":" t)
                                         for path = (concat d "/" dir "/")
                                         if (file-directory-p path)
                                         return path)
                                dir)))
    (compile command)
    nil))

(defun org-babel-prep-session:compile (_session _params)
  (error "Compile does not support sessions"))

(defvar ob-compile-org-file nil
  "Default file to use for `ob-compile'.")

(defun ob-compile ()
  (interactive)
  (or ob-compile-org-file
      (setq ob-compile-org-file (read-file-name "File: " nil nil t)))
  (let* ((completion-ignore-case t)
	 (case-fold-search t)
	 (all-block-names (org-babel-src-block-names ob-compile-org-file))
         (org-babel-execute:default-directory default-directory)
         (name (completing-read
	        "source-block name: " all-block-names nil t)))
    (with-current-buffer (find-file-noselect ob-compile-org-file)
      (goto-char (org-babel-find-named-block name))
      (org-babel-execute-src-block
       nil
       (org-babel-get-src-block-info nil (org-element-context))))))

(provide 'ob-compile)
