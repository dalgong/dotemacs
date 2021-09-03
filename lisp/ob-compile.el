;; A simple org babel for compile
;; #+NAME: expand-src-block
;; #+BEGIN_SRC elisp :var block-name="" :var datum="" :var info="" :var lang="" :var body="" :exports none
;;   (save-excursion
;;     (org-babel-goto-named-src-block block-name)
;;     (setq datum (org-element-at-point))
;;     t)
;;   (setq info (org-babel-get-src-block-info nil datum))
;;   (cl-callf org-babel-merge-params (nth 2 info) params)
;;   (cl-callf org-babel-process-params (nth 2 info))
;;   (setq lang (nth 0 info))
;;   (setq body (org-babel-expand-src-block nil info))
;;   (format "%s" body)
;; #+END_SRC

(require 'ob)

(defvar org-babel-default-header-args:compile
  '((:dir . nil) (:results . "silent"))
  "Default arguments to use when evaluating a compile source block.")

(defvar org-babel-execute:default-directory nil)
(defvar org-babel-execute:compile-dir-expander nil)

(defun org-babel-expand-body:compile (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params)))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
             (value (cdr pair)))
         (setq body
               (replace-regexp-in-string
                (concat "$" (regexp-quote name))
                (if (stringp value) value (format "%S" value))
                body
                t
                t))))
     vars)
    body))

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
