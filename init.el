;; -*- lexical-binding: t -*-
;; jay+nospam@kldp_remove_me_.org
(setq custom-file "~/.emacs.d/custom.el")
(add-to-list 'load-path "~/.emacs.d/lisp")

(when (require 'package nil t)
  (custom-set-variables
   '(package-archives
     '(("gnu"   . "http://elpa.gnu.org/packages/")
       ("melpa" . "http://melpa.org/packages/")
       ("org"   . "http://orgmode.org/elpa/"))))
  (when (< emacs-major-version 27)
    (condition-case _ (package-initialize)
      (error (package-refresh-contents)
             (package-initialize))))
  (unless (require 'use-package nil t)
    (package-refresh-contents)
    (package-install 'use-package)))

(defvar use-dark-mode t)
(defvar completion-framework 'icomplete)      ; '(ivy helm ido icomplete)
(defvar shell-variant 'shell)           ; '(eshell shell)

(when (and t (file-directory-p "~/work/nano-emacs"))
  ;; git clone https://github.com/rougier/nano-emacs.git
  (add-hook 'emacs-startup-hook
            (defun load-nano-emacs-setup ()
              (add-to-list 'load-path (expand-file-name "~/work/nano-emacs"))
              (require 'nano-faces)
              (nano-faces)
              (if use-dark-mode
                  (require 'nano-theme-dark)
                (require 'nano-theme-light))
              (require 'nano-modeline)
              (set-face-background 'mode-line 'unspecified)
              (set-face-background 'mode-line-inactive 'unspecified))))
(eval-and-compile
  (defmacro csetq (sym val)
    `(funcall (or (get ',sym 'custom-set) 'set-default) ',sym ,val)))
(use-package bind-key
  :ensure)
(use-package diminish
  :ensure
  :config
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'flymake-mode))
(use-package simple
  :custom
  (ad-redefinition-action 'accept)
  (async-shell-command-buffer 'rename-buffer)
  (auto-window-vscroll nil)
  (backup-by-copying t)
  (backup-directory-alist '(("." . "~/z/backups")))
  (bidi-display-reordering nil)
  (bidi-inhibit-bpa t)
  (bidi-paragraph-direction 'left-to-right)
  (blink-matching-paren t)
  (calc-display-trail nil)
  (create-lockfiles nil)
  (delete-old-versions t)
  (disabled-command-function nil)
  (display-buffer-alist '(("\\*shell\\*" display-buffer-same-window)
                          ("\\*compilation\\*" display-buffer-in-bottom-window)))
  (enable-recursive-minibuffers t)
  (eval-expression-print-length nil)
  (eval-expression-print-level nil)    
  (even-window-heights nil)
  (fast-but-imprecise-scrolling t)
  (fit-window-to-buffer-horizontally t)
  (font-lock-maximum-decoration '((c-mode . 2) (c++-mode . 2) (t . t)))
  (frame-resize-pixelwise t)
  (help-char ?^)
  (history-length 1000)
  (completion-styles '(initials partial-completion flex))
  (completion-cycle-threshold 10)
  (idle-update-delay 1)
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (kill-buffer-query-functions nil)
  (kill-read-only-ok t)
  (kill-ring-max 3000)
  (kill-whole-line t)
  (line-move-visual nil)
  (mac-option-key-is-meta t)
  (mac-right-option-modifier nil)
  (mark-even-if-inactive t)
  (max-mini-window-height 0.15)
  (mode-line-client '(""))
  (mode-line-modified '("%* "))
  (mode-line-remote '(""))
  (mode-line-frame-identification '(""))
  (mode-line-front-space '("  "))
  (mode-line-position '(""))
  (mode-line-mule-info '(""))
  (mode-line-end-spaces
   '(:eval (concat (propertize " " 'display `(space :align-to (- right 15)))
                   (let* ((tabs (frame-parameter nil 'tabs))
                          (current (cdr (assq 'name (and (> (length tabs) 1) (assq 'current-tab tabs))))))
                     (if current
                         (concat "[" current "]")
                       ""))
                   (propertize " " 'display `(space :align-to (- right 6)))
                   "%l:%c")))
  (ns-alternate-modifier 'super)
  (ns-command-modifier 'meta)
  (ns-tool-bar-display-mode 'both)
  (ns-tool-bar-size-mode 'regular)
  (ns-use-thin-smoothing t)
  (prefix-help-command 'dispatch-command-with-prefix)
  (process-adaptive-read-buffering nil)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-process-output-max (* 1024 1024))
  (resize-mini-windows 'grow-only)
  (ring-bell-function 'ignore)
  (scroll-conservatively 1)
  (scroll-preserve-screen-position t)
  (scroll-margin 0)
  (scroll-step 1)
  (select-active-regions nil)
  (sentence-end-double-space nil)
  (shell-command-switch "-lc")  
  (split-height-threshold nil)
  (split-width-threshold nil)
  (suggest-key-bindings nil)
  (tab-always-indent 'complete)
  (truncate-lines t)
  (use-dialog-box nil)
  (use-package-compute-statistics nil)
  (vc-follow-symlinks t)
  (version-control t)
  (x-selection-timeout 100)
  (window-resize-pixelwise t)
  :init (defvar goto-line*-map (make-sparse-keymap))
  :bind (("M-0"                . delete-window)
         ("M-1"                . delete-other-windows)
         ("M-2"                . split-window-below)
         ("M-3"                . split-window-right)
         ([C-tab]              . other-window)
         ([C-up]               . windmove-up)
         ([C-down]             . windmove-down)
         ([C-left]             . windmove-left)
         ([C-right]            . windmove-right)
         ([remap suspend-frame]. ignore)
         ([remap kill-buffer]  . kill-this-buffer)
         ("C-TAB"              . other-window)
         ("C-."                . next-error)
         ("C-,"                . previous-error)
         ("C-RET"              . open-dwim)
         ("M-SPC"              . cycle-spacing)
         ("M-g"                . goto-line*)
         ("M-K"                . kill-this-buffer)
         ("M-o"                . other-window)
         ("M-q"                . fill-paragraph)
         ("M-s"                . save-buffer)
         ("RET"                . newline-and-indent)
         ("M-9"                . open-dwim)
         ("<mouse-2>"          . mouse-run-command-dwim)
         ("<mouse-3>"          . mouse-open-dwim)

         ("M-n"                . forward-paragraph)
         ("M-p"                . backward-paragraph)

         :map goto-line*-map
         ("^"                  . goto-line*-dispatch)
         ("?"                  . goto-line*-dispatch)
         ("n"                  . next-error)
         ("p"                  . previous-error)
         ("M-o"                . open-dwim)

         :map help-map
         ("1"                  . byte-compile-file)
         ("2"                  . package-list-packages-no-fetch)
         ("3"                  . package-install)

         :map ctl-x-map
         ("k"                  . kill-this-buffer)
         ("O"                  . ff-find-other-file)

         :map mode-specific-map
         ("`"                  . shell-command)
         ("1"                  . shell-command)
         ("7"                  . async-shell-command)
         ("\\"                 . shell-command-on-region)
         ("9"                  . compile)
         ("0"                  . recompile)
         ("-"                  . recursive-edit*)
         ("j"                  . join-region-into-one-line)
         ("b"                  . bury-buffer)
         ("q"                  . quick-calc)
         ("u"                  . rename-uniquely*)           
         ("E"                  . erase-buffer)

         :map isearch-mode-map
         ("M-s ?" . isearch-mode-map-dispatch))
  :bind-keymap
  ("M-4" . ctl-x-4-map)
  :config
  (find-function-setup-keys)
  (defun open-dwim (path)
    (interactive
     (save-excursion
       (let ((candidate (and (require 'ffap) (ffap-guesser)))
             start)
         (when candidate
           (if (not (or (looking-at (regexp-quote candidate))
                        (progn
                          (goto-char (point-at-eol))
                          (search-backward candidate (point-at-bol) t))))
               (list candidate)
             (setq start (point))
             (forward-char (length candidate))
             (re-search-forward ":\\(\\([0-9]+\\):?\\([0-9]*\\)\\|/[^/]+/\\)" (point-at-eol) t)
             (list (buffer-substring-no-properties start (point))))))))
    (if path
        (if (ffap-url-p path)
            (funcall ffap-url-fetcher path)
          (let (line col search)
            (when (string-match "^\\(.*?\\):\\(\\([0-9]+\\):?\\([0-9]*\\)\\|/\\([^/]+\\)/\\)$" path)
              (setq line   (and (match-string 3 path) (string-to-number (match-string 3 path)))
                    col    (and (match-string 4 path) (string-to-number (match-string 4 path)))
                    search (match-string 5 path))
              (setq path   (match-string 1 path)))
            (find-file-other-window path)
            (goto-char (point-min))
            (cond (search
                   (when (re-search-forward search)
                     (goto-char (match-beginning 0))))
                  (line
                   (forward-line (1- line))
                   (when (and col (> col 0))
                     (forward-char (1- col)))))))
      (call-interactively 'ffap-other-window)))
  (defun mouse-run-command-dwim ()
    (interactive)
    (let ((s (get-current-active-selection)))
      (cond (s
             (compile s))
            ((eq major-mode 'org-mode)
             (call-interactively 'mouse-set-point)
             (call-interactively 'org-ctrl-c-ctrl-c))
            ((eq major-mode 'shell-mode)
             (call-interactively 'mouse-set-point)
             (call-interactively 'comint-copy-old-input)
             (call-interactively 'comint-send-input))
            (t
             (call-interactively 'mouse-set-point)
             (compile (thing-at-point 'filename))))))
  (defun mouse-open-dwim ()
    (interactive)
    (let ((s (get-current-active-selection)))
      (cond (s
             (open-dwim s))
            (t
             (call-interactively 'mouse-set-point)
             (call-interactively 'open-dwim)))))
  (defun always-use-bottom-window (_ &optional height)
    ;; Open helm window deterministic location always.
    ;; (while (window-in-direction 'left)  (select-window (window-in-direction 'left)))
    (setq height (or height (symbol-value 'helm-display-buffer-default-height)))
    (while (window-in-direction 'below) (select-window (window-in-direction 'below)))
    (when (> (window-height (selected-window)) (+ 5 height))
      (split-window (selected-window) (- -2 height) 'below)
      (select-window (window-in-direction 'below))))
  (defun display-buffer-in-bottom-window (buffer _)
    (let ((w (get-buffer-window buffer)))
      (unless w
        (always-use-bottom-window nil 30)
        (setq w (selected-window)))
      (window--display-buffer buffer w 'reuse)))
  (fset 'yes-or-no-p 'y-or-n-p)
  (defvar dispatch-command-from-keymap-map nil)
  (defun dispatch-command-from-keymap (keymap &optional excludes)
    (setq dispatch-command-from-keymap-map keymap)
    (let* ((lines (cdr (split-string (substitute-command-keys "\\{dispatch-command-from-keymap-map}") "\n" t)))
           (key-pos (+ 2 (length (car (split-string (car lines) "  -"))))))
      (setq lines (cdr lines))
      (let (r p)
        (dolist (l lines)
          (when (and (> (length l) key-pos)
                     (not (string= (setq p (substring l key-pos)) "Prefix Command"))
                     (not (memq (setq p (intern p)) excludes)))
            (push (cons l p) r)))
        (setq r (nreverse r))
        (when (and (setq p (completing-read "Command: " r))
                   (setq p (assoc p r)))
          (call-interactively (cdr p))))))
  (defun dispatch-command-with-prefix (&optional _)
    (interactive)
    (let ((key (this-command-keys)))
      (if (stringp key)
          (setq key (substring key 0 (1- (length key))))
        (let ((prefix (make-vector (1- (length key)) nil))
              (i 0))
          (while (< i (length prefix))
            (aset prefix i (aref key i))
            (setq i (1+ i)))
          (setq key prefix)))
      (dispatch-command-from-keymap (key-binding key))))
  (defun isearch-mode-map-dispatch ()
    (interactive)
    (dispatch-command-from-keymap isearch-mode-map))
  (defun get-current-active-selection ()
    (let ((p (if (use-region-p)
                 (cons (region-beginning) (region-end))
               (and (fboundp 'easy-kill--bounds)
                    (ignore-errors (funcall 'easy-kill--bounds))))))
      (and p (car p) (buffer-substring-no-properties (car p) (cdr p)))))

  (defun call-other-window-if-interactive (&rest _)
    (when (called-interactively-p 'interactive)
      (other-window 1)))
  (advice-add 'split-window-right :after #'call-other-window-if-interactive)
  (advice-add 'split-window-below :after #'call-other-window-if-interactive)

  (advice-add 'kill-this-buffer :around
              (defun kill-this-buffer-really (_ &rest _r)
                (kill-buffer (current-buffer))))
  (defun join-region-into-one-line ()
    (interactive)
    (let ((old-fill-column fill-column))
      (setq fill-column most-positive-fixnum)
      (unwind-protect (fill-paragraph nil t)
        (beginning-of-line)
        (setq fill-column old-fill-column))))
  (defvar goto-line*-modes '(display-line-numbers-mode hl-line-mode))
  (defvar goto-line*-buffer nil)
  (defvar goto-line*-old-values nil)
  (defvar goto-line*-timer nil)
  (cl-flet ((safe-eval (symbol) (if (and (boundp symbol) (symbol-value symbol)) 1 -1))
            (safe-call (fn v) (and (fboundp fn) (funcall fn v))))
    (defun goto-line*-setup ()
      (unless (fboundp 'display-line-numbers-mode)
        (require 'linum)
        (fset 'display-line-numbers-mode 'linum-mode))
      (with-current-buffer goto-line*-buffer
        (setq goto-line*-buffer (current-buffer)
              goto-line*-old-values (mapcar #'safe-eval goto-line*-modes))
        (cl-mapc #'safe-call goto-line*-modes '(1 1))))
    (defun goto-line*-cleanup ()
      (when  goto-line*-timer
        (cancel-timer goto-line*-timer)
        (setq goto-line*-timer nil))
      (with-current-buffer goto-line*-buffer
        (cl-mapc #'safe-call goto-line*-modes goto-line*-old-values)
        (setq goto-line*-buffer nil
              goto-line*-old-values nil)))
    (defun goto-line* ()
      (interactive)
      (setq goto-line*-buffer (current-buffer)
            goto-line*-timer (run-with-timer .3 nil #'goto-line*-setup))
      (let (cmd k)
        (unwind-protect
            (unless (setq cmd (and (setq k (let ((overriding-local-map goto-line*-map))
                                             (read-key-sequence "Goto line: ")))
                                   (lookup-key goto-line*-map k)))
              (setq unread-command-events
                    (nconc (listify-key-sequence k) unread-command-events))
              (call-interactively 'goto-line))
          (goto-line*-cleanup))
        (when cmd (call-interactively cmd)))))
  (defun goto-line*-dispatch ()
    (interactive)
    (dispatch-command-from-keymap goto-line*-map '(goto-line*-dispatch)))

  (defun rename-uniquely* ()
    (interactive)
    (let ((tag (read-string "tag: "))
          (buffer-name (buffer-name)))
      (when (string-match "^\\([^<>]*\\)\\(<[^>]*>\\)?\\(.*\\)$" buffer-name)
        (let ((s (match-string 1 buffer-name))
              (e (match-string 3 buffer-name)))
          (if (and (string= tag "") (null (match-string 2 buffer-name)))
              (rename-uniquely)
            (rename-buffer (if (string= tag "") (concat s e)
                             (format "%s<%s>%s" s tag e))
                           t))))))
  (defun recursive-edit* ()
    (interactive)
    (save-window-excursion
      (recursive-edit)))

  (advice-add 'async-shell-command :before
              (defun uniqify-running-shell-command (&rest _)
                (let ((buf (get-buffer "*Async Shell Command*")))
                  (if buf
                      (let ((proc (get-buffer-process buf)))
                        (if (and proc (eq 'run (process-status proc)))
                            (with-current-buffer buf
                              (rename-uniquely))))))))
  (advice-add 'bounds-of-thing-at-point :around
              (defun handle-paragraph (o thing)
                (if (eq thing 'paragraph)
                    (save-excursion
                      (let ((p (progn (forward-paragraph) (point))))
                        (backward-paragraph)
                        (cons (point) p)))
                  (funcall o thing))))
  (advice-add 'browse-url-interactive-arg :filter-return
              (defun google-if-not-a-url (r)
                (and r (car r)
                     (not (string-match "^https?://" (car r)))
                     (> (length (car r)) 0)
                     (setcar r
                             (if (string-match "^[a-z0-9]+/" (car r))
                                 (concat "http://" (car r))
                               (concat "https://www.google.com/search?q="
                                       (mapconcat
                                        #'identity
                                        (mapcar #'url-hexify-string
                                                (split-string (car r) " "))
                                        "+")))))
                r))
  (advice-add 'display-startup-echo-area-message :override
              (lambda (&rest _) "Silent startup!"))
  (setq mode-line-modes (remove ")" (remove "(" mode-line-modes)))
  (defun pulse-line (&rest _)
    (pulse-momentary-highlight-one-line (point)))
  (dolist (c '(scroll-up-command scroll-down-command
                                 cua-scroll-up cua-scroll-down
                                 recenter-top-bottom other-window ivy-switch-buffer))
    (advice-add c :after #'pulse-line)))
(use-package files
  :custom
  (backup-by-copying-when-linked t)
  (backup-by-copying-when-mismatch t)
  (confirm-kill-emacs nil)
  (confirm-nonexistent-file-or-buffer nil)
  (require-final-newline t)
  :config
  (put 'backup-inhibited 'safe-local-variable 'booleanp)
  (defvar server-buffer-clients nil)
  (advice-add 'save-buffers-kill-terminal :around
              (defun do-server-edit-if-server-buffer (o &rest args)
                (if server-buffer-clients
                    (call-interactively 'server-edit)
                  (apply o args))))
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))
(use-package faces
  :init
  (unless (display-graphic-p)
    (advice-add #'tty-run-terminal-initialization :override #'ignore)
    (add-hook 'window-setup-hook
              (defun restore-tty-run-terminal-initialization ()
                (advice-remove #'tty-run-terminal-initialization #'ignore)
                (tty-run-terminal-initialization (selected-frame) nil t))))
  :config
  (add-hook 'after-make-frame-functions
            (defun adjust-default-color (frame)
              (modify-frame-parameters frame default-frame-alist))))
(use-package auto-highlight-symbol
  :ensure
  :diminish
  :hook (after-init . global-auto-highlight-symbol-mode)
  :bind (:map auto-highlight-symbol-mode-map
              ("C-x '" . ahs-change-range)))
(use-package avy
  :ensure
  :bind ("C-'" . avy-goto-char-timer)
  :config
  (advice-add 'avy-goto-char-timer :around
              (defun avy-pop-mark-if-prefix (o &rest args)
                (if current-prefix-arg
                    (call-interactively 'avy-pop-mark)
                  (apply o args)))))
(use-package bash-completion
  :ensure
  :hook (after-init . bash-completion-setup))
(use-package browse-url
  :functions browse-url-url-at-point
  :config
  (defun fix-browse-url-interactive-arg (_ prompt)
    (let ((event (ignore-errors (elt last-command-event 0))))
      (and (listp event) (mouse-set-point event)))
    (list (read-string prompt (or (and transient-mark-mode mark-active
                                       ;; rfc2396 Appendix E.
                                       (replace-regexp-in-string
                                        "[\t\r\f\n ]+" ""
                                        (buffer-substring-no-properties
                                         (region-beginning) (region-end))))
                                  (browse-url-url-at-point)))
          (not (eq (null browse-url-new-window-flag)
                   (null current-prefix-arg)))))
  (advice-add 'browse-url-interactive-arg :around #'fix-browse-url-interactive-arg))
(use-package cc-mode
  :custom
  (c-electric-flag nil)
  :hook (c-mode-common . set-outline-regexp)
  :config
  (defun set-outline-regexp ()
    (require 'outline)
    (setq outline-regexp "\\s-*\\S-"))
  ;; c-tab-always-indent does not support 'complete of tab-always-indent.
  (defun may-complete-symbol (o &rest args)
    (let ((op (point)))
      (apply o args)
      (when (and (eq this-command 'c-indent-line-or-region)
                 (eq op (point)))
        (call-interactively 'company-complete))))
  (advice-add 'c-indent-line :around #'may-complete-symbol)
  (defun do-self-insert-command (_1 &rest _2)
    (interactive)
    (call-interactively 'self-insert-command))
  (advice-add 'c-electric-brace :around #'do-self-insert-command)
  (defun merge-empty-lines-with-prevous-line (_)
    (let (buffer-invisibility-spec) ;; This so that `current-column' DTRT
      ;; in otherwise-hidden text.
      (save-excursion
        (if (eolp)
            (progn
              (beginning-of-line 0)
              (if (bobp) 0 (merge-empty-lines-with-prevous-line nil)))
          (skip-chars-forward "\t ")
          (current-column)))))
  (advice-add 'c-outline-level :around #'merge-empty-lines-with-prevous-line))
(use-package color-identifiers-mode
  :ensure
  :diminish
  :hook (after-init . global-color-identifiers-mode))
(use-package comint
  :bind (:map comint-mode-map
              ([C-up]   . nil)
              ([C-down] . nil))
  :hook (comint-output-filter-functions . (comint-watch-for-password-prompt comint-truncate-buffer))
  :custom
  (comint-buffer-maximum-size 10240)
  (comint-move-point-for-output nil)
  (comint-scroll-to-bottom-on-input nil)
  :config
  ;; (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
  ;; (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
  (defun do-silently (o &rest args)
    (let ((message (symbol-function 'message)))
      (unwind-protect
          (progn (fset 'message 'ignore) (apply o args))
        (fset 'message message))))
  (advice-add 'comint-previous-matching-input :around #'do-silently))
(use-package company
  :ensure
  :diminish
  :hook (after-init . global-company-mode)
  :bind (:map prog-mode-map
         ("C-i"   . company-indent-or-complete-common)
         ("C-M-i" . counsel-company)
         :map mode-specific-map
         ("SPC"   . company-complete)
         ("C-SPC" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("M-s" . company-filter-candidates))
  :custom
  ;; (company-auto-complete t)
  (company-idle-delay nil)
  (company-tooltip-idle-delay nil)
  (company-tooltip-align-annotations t)
  :config
  (advice-add 'cua-set-mark :around #'company-complete-dwim)
  (defun company-complete-dwim (o &rest args)
    (if (or (not (called-interactively-p 'interactive))
            current-prefix-arg
            (memq last-command '(cua-set-mark pop-to-mark-command pop-global-mark))
            (not (sit-for 0.5)))
        (apply o args)
      (call-interactively 'company-complete))))
(use-package compile
  :diminish compilation-in-progress
  :hook ((compilation-mode . run-before-compile)
         (compilation-filter . apply-xterm-color-filter))
  :bind (("<f7>" . compile)
         ("<f8>" . recompile)
         :map compilation-mode-map
         ("M-{" . nil)
         ("M-}" . nil)
         ("." . rename-uniquely*)
         ("t" . toggle-truncate-lines)
         ([remap read-only-mode] . compilation-toggle-shell-mode))
  :custom
  (compilation-environment '("TERM=xterm-256color"))
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-save-buffers-predicate (lambda ()))
  ;; non-nil significantly slows down emacs
  (compilation-scroll-output nil)
  (completion-auto-help 'lazy)
  (completion-styles '(partial-completion initials))
  (completion-pcm-complete-word-inserts-delimiters t)
  :config
  (defun apply-xterm-color-filter ()
    (let* ((proc (get-buffer-process (current-buffer)))
           (end-marker (and proc (process-mark proc))))
      (goto-char compilation-filter-start)
      (while (search-forward "\033[1A\033[K" end-marker t)
        (let ((pos (point-at-bol 0)))
          (save-excursion
            (goto-char pos)
            (while (looking-at "^ ")
              (beginning-of-line 0))
            (setq pos (point)))
          (setq compilation-filter-start (min compilation-filter-start pos))
          (delete-region pos (point))))
      (goto-char end-marker)
      (let* ((s (buffer-substring-no-properties compilation-filter-start end-marker))
             (ns (xterm-color-filter s)))
        (unless (string-equal s ns)
          (delete-region compilation-filter-start end-marker)
          (insert (xterm-color-filter s))))
      (set-marker end-marker (point))))
  (defun ascend-to-directory-with-file (file &optional dir)
    (setq dir (expand-file-name (or dir default-directory)))
    (while (and (not (file-exists-p (concat dir file)))
                (not (string= dir "/")))
      (setq dir (file-name-directory (directory-file-name dir))))
    (and (file-exists-p (concat dir file)) dir))
  (advice-add 'compilation-start :around
              (defun compilation-start-ascend-to-rust-topdir (o command &rest args)
                (let ((default-directory default-directory)
                      dir)
                  (when (and (string-match "^cargo " command)
                             (setq dir
                                   (ascend-to-directory-with-file "Cargo.toml")))
                    (setq default-directory dir))
                  (apply o command args))))
  (advice-add #'recompile :around
              (defun do-kill-compilation (o &rest args)
                (when (and (called-interactively-p 'interactive)
                           (eq major-mode 'compilation-mode)
                           (get-buffer-process (current-buffer)))
                  (kill-compilation))
                (apply o args)))
  (setq compilation-error-regexp-alist
        (cl-set-difference
         compilation-error-regexp-alist
         '(4bsd absoft ada aix ant borland comma cucumber edg-1 edg-2 epc
                gcov-called-line gcov-file gcov-header gcov-never-called gcov-nomark
                iar ibm irix jikes-file jikes-line lcc makepp maven mips-1 mips-2 msft
                omake oracle php rxp sparc-pascal-example sparc-pascal-file sparc-pascal-line
                sun sun-ada watcom weblint
                guile-line)))
  (defun run-before-compile ()
    (let ((buffer (compilation-buffer-name mode-name major-mode nil)))
      (when (get-buffer buffer)
        (buffer-disable-undo (get-buffer buffer)))))
  (defun compilation-toggle-shell-mode ()
    (interactive) (setq buffer-read-only nil)
    (shell-mode)))
(use-package cua-base
  :hook (after-init . cua-mode)
  :custom
  (cua-delete-selection t)
  (cua-enable-cua-keys nil)
  (cua-prefix-override-inhibit-delay 0.7))
(use-package dabbrev
  :bind (("C-M-_" . dabbrev-completion)
         ("C-M-/" . dabbrev-completion)
         :map mode-specific-map
         ("/" . dabbrev-expand))
  :custom
  (dabbrev-case-fold-search nil)
  :config
  (advice-add 'dabbrev--find-expansion :around
              (defun suppress-message (o &rest args)
                (let ((inhibit-message t))
                  (apply o args)))))
(use-package deferred
  :ensure)
(use-package diffview
  :ensure
  :after diff-mode
  :bind (:map diff-mode-map
              ("|" . diffview-current))
  :config
  (advice-add 'scroll-all-check-to-scroll :after
              (defun handle-cua-scroll-commands (&rest _)
                (cond ((eq this-command 'cua-scroll-up)
                       (scroll-all-function-all 'cua-scroll-up nil))
                      ((eq this-command 'cua-scroll-down)
                       (scroll-all-function-all 'cua-scroll-down nil))))))
(use-package dired
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("l" . dired-up-directory))
  :custom
  (dired-no-confirm t)
  (dired-use-ls-dired nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always))
(use-package dired-aux
  :after dired
  :bind (:map dired-mode-map
              ([remap dired-do-find-regexp] . dired-do-multi-occur))
  :config
  (defun dired-do-multi-occur (regexp)
    "Run `dired-do-multi-occur` with REGEXP on all marked files."
    (interactive (list (read-regexp "Regexp: ")))
    (multi-occur (mapcar 'find-file-noselect (dired-get-marked-files)) regexp)))
(use-package dired-subtree
  :ensure
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)))
(use-package dired-x
  :after dired
  :hook (dired-mode . dired-extra-startup))
(use-package display-line-numbers
  :disabled
  :hook ((prog-mode conf-mode) . display-line-numbers-mode)
  :custom
  (display-line-numbers-width 3)
  (display-line-numbers-widen t))
(use-package docker
  :ensure
  :bind (:map goto-line*-map ("M-d" . docker)))
(use-package easy-kill
  :ensure
  :bind (([remap kill-ring-save]              . easy-kill)
         :map easy-kill-base-map
         ([remap cua-exchange-point-and-mark] . easy-kill-exchange-point-and-mark)
         ([remap cua-set-mark]                . easy-kill-mark-region)
         ("o" . easy-kill-expand)
         ("i" . easy-kill-shrink)))
(use-package easy-repeat
  :ensure
  :hook (after-init . easy-repeat-mode)
  :config
  (dolist (c '(goto-last-change tab-next tab-previous tab-move))
    (cl-pushnew c easy-repeat-command-list)))
(use-package ediff
  :bind (:map mode-specific-map ("=" . ediff-current-file))
  :custom
  (diff-switches "-wu")
  (ediff-diff-options "-w")
  (ediff-custom-diff-options "-u")
  (ediff-keep-variants nil)
  (ediff-highlight-all-diffs 'nil)
  :hook ((ediff-before-setup . save-window-configuration)
         ((ediff-quit ediff-suspend) . restore-window-configuration))
  :config
  (defun say-yes (o &rest args)
    (cl-flet ((y-or-n-p (_) t))
      (apply o args)))
  (advice-add #'ediff-quit-meta-buffer :around #'say-yes)
  (advice-add #'ediff-quit             :around #'say-yes)
  (advice-add #'ediff-janitor          :filter-args (defun dont-ask (args) (setcar args nil) args))
  (defvar ediff-saved-window-configurations nil)
  (defun save-window-configuration ()
    (setq ediff-saved-window-configurations (current-window-configuration)))
  (defun restore-window-configuration ()
    (when (window-configuration-p ediff-saved-window-configurations)
      (set-window-configuration ediff-saved-window-configurations)))
  (use-package ediff-wind
    :custom
    (ediff-split-window-function #'split-window-horizontally)
    (ediff-window-setup-function #'ediff-setup-windows-plain)))
(use-package eldoc
  :hook ((lisp-interaction-mode emacs-lisp-mode python-mode) . turn-on-eldoc-mode))
(use-package elfeed
  :bind (("C-x !" . elfeed)))
(use-package eshell
 :if (eq shell-variant 'eshell)
 :bind (("C-`"  . eshell)
        ("C-\\" . eshell)
        ("C-;"  . eshell))
 :hook (eshell-mode . eshell-smart-initialize)
 :custom
 (helm-show-completion-display-function #'helm-show-completion-default-display-function)
 (eshell-hist-ignoredups t)
 :config
 (require 'em-smart)
 (advice-add 'eshell-list-history :override 'helm-eshell-history)
 (advice-add 'eshell-source-file :around
             (defun source-bash-if-shell (o file &rest args)
               (if (string-match "[.]sh\\'" file)
                   (throw 'eshell-replace-command
                          (eshell-parse-command
                           "/bin/bash"
                           (list "-c" (concat ". " (mapconcat #'identity (cons file (car args)) " ")))))
                 (apply o file args)))))
(use-package esh-autosuggest
  :ensure
  :hook (eshell-mode . esh-autosuggest-mode))
(use-package evil
  :disabled
  :ensure
  :hook (after-init . evil-mode)
  :custom
  (evil-default-state 'emacs)
  (evil-ex-search-highlight-all nil))
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure
  :hook (after-init . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (dolist (env (split-string (shell-command-to-string "bash -lc env") "\n" t))
    (let* ((p (split-string env "=" nil))
           (name (car p))
           (values (cdr p)))
      (unless (member name exec-path-from-shell-variables)
        (setenv name (mapconcat #'identity values "="))))))
(use-package flx
  :ensure)
(use-package gcmh
  :ensure
  :diminish
  :hook (after-init . gcmh-mode))
(use-package go-mode
  :ensure
  :custom
  (gofmt-command "goimports")
  (go-fontify-function-calls nil)
  (godoc-use-completing-read t)
  :bind (:map go-mode-map ("C-h d" . godoc))
  :hook ((go-mode . go-mode-setup)
         (go-mode . eglot-ensure))
  :config
  ;; go get golang.org/x/tools/cmd/...
  ;; go get github.com/rogpeppe/godef
  ;; go get github.com/nsf/gocode
  ;; go get golang.org/x/tools/cmd/goimports
  ;; go get golang.org/x/tools/gopls
  (add-to-list 'exec-path (expand-file-name "~/go/bin"))
  (defun go-mode-setup ()
    (add-hook 'before-save-hook 'gofmt-before-save)
    (unless (string-match "go" compile-command)
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))))
(use-package goto-last-change
  :ensure
  :bind (:map goto-line*-map ("." . goto-last-change)))
(use-package helm
  :if (eq completion-framework 'helm)
  :ensure)
(use-package helm-mode
  :diminish
  :hook ((helm-after-initialize . helm-hide-mode-line))
  :bind (("M-P"       . helm-mini)
         
         :map helm-map
         ("TAB"       . helm-execute-persistent-action)
         ("C-i"       . helm-execute-persistent-action)
         ("<backtab>" . helm-previous-line)
         ("M-i"       . helm-previous-line)
         ("M-s"       . helm-next-source)
         ("M-r"       . helm-previous-source)
         ("M-/"       . helm-select-action)
         ("M-["       . helm-enlarge-window)
           
         :map help-map
         ("SPC"       . helm-all-mark-rings)
         ("/"         . helm-dabbrev)
         ("="         . helm-calcul-expression)
         ("C-z"       . helm-toggle-suspend-update)
         ("a"         . helm-apropos)
         ("b"         . helm-descbinds)
         ("g"         . helm-google-suggest)
         ("o"         . helm-top)
         ("q"         . helm-regexp)
         ("r"         . helm-register)
         ("z"         . helm-resume)

         :map isearch-mode-map
         ("M-s M-r"   . helm-rg-from-isearch)
         ("M-s M-s"   . helm-occur-from-isearch)
         ("M-s M-o"   . helm-multi-occur-from-isearch))
  :custom
  (helm-left-margin-width 2)
  (helm-buffers-left-margin-width 2)
  (helm-split-window-preferred-function #'always-use-bottom-window)
  (helm-split-window-inside-p nil)
  (helm-always-two-windows nil)
  (helm-display-buffer-default-height 10)
  (helm-candidate-number-limit 7)
  (helm-display-header-line nil)
  (helm-echo-input-in-header-line nil)
  (helm-ff-file-name-history-use-recentf t)
  (helm-ff-keep-cached-candidates nil)
  (helm-M-x-fuzzy-match t)
  (helm-mini-default-sources
   '(helm-source-buffers-list
     helm-source-recentf
     ;; helm-source-buffer-not-found
     ))
  :custom-face
  (helm-header ((t :inherit header-line)))
  (helm-selection ((t :inherit hl-line)))
  (helm-selection-line ((t :inherit hl-line)))
  :config
  (when (eq completion-framework 'helm)
    (helm-mode t)
    (bind-keys
     ("M-T"       . helm-semantic-or-imenu)
     ("M-y"       . helm-show-kill-ring)
     ("M-x"       . helm-M-x)
     ([remap find-file] . helm-find-files)
     ([remap switch-to-buffer] . helm-mini)
     :map mode-specific-map
     ("g"         . helm-rg)))
  (fset 'helm-display-mode-line #'ignore)
  (defun helm-hide-mode-line ()
    (with-current-buffer helm-buffer
      (setq-local mode-line-format nil)))
  (when (eq completion-framework 'helm)
    (add-hook 'after-init-hook #'helm-mode)))
(use-package helm-dash
  :if (eq completion-framework 'helm)
  :ensure
  :bind (:map help-map (("d" . helm-dash)))
  :custom
  (dash-docs-browser-func 'eww))
(use-package helm-files
  :if (eq completion-framework 'helm)
  :bind (([remap find-file] . helm-find-files)
         :map ctl-x-map
         ("C-r"   . helm-recentf)
         :map helm-find-files-map
         ("<backspace>" . maybe-helm-find-files-up-one-level)
         ("/"           . maybe-helm-execute-persistent-action))
  :config
  (defun maybe-helm-find-files-up-one-level (arg)
    (interactive "p")
    (if (string-match "/$" helm-pattern)
        (helm-find-files-up-one-level arg)
      (backward-delete-char-untabify arg)))
  (defun maybe-helm-execute-persistent-action ()
    (interactive)
    (if (string-match "[^~/]+$" helm-pattern)
        (call-interactively 'helm-execute-persistent-action)
      (call-interactively 'self-insert-command)))
  (advice-add #'helm-ff-find-sh-command :override #'helm-ff-recursive-files)
  (defvar helm-source-ff-recursive-files nil)
  (defun helm-ff-recursive-files (_)
    (require 'helm-find)
    (unless helm-source-ff-recursive-files
      (setq helm-source-ff-recursive-files
            (helm-build-async-source "Recursive Files"
              :header-name (lambda (name)
                             (concat name " in [" (helm-default-directory) "]"))
              :candidates-process
              (lambda ()
                (start-process-shell-command
                 "rg" nil (format "rg --files %s | egrep '%s'"
                                  (helm-default-directory) helm-pattern)))
              :action 'helm-type-file-actions
              :help-message 'helm-generic-file-help-message
              :keymap (symbol-value 'helm-find-map))))
    (with-helm-default-directory helm-ff-default-directory
      (helm :sources 'helm-source-ff-recursive-files
            :buffer "*helm recursive files*"
            :ff-transformer-show-only-basename nil
            :case-fold-search helm-file-name-case-fold-search))))
(use-package helm-swoop
  :if (eq completion-framework 'helm)
  :ensure
  :bind (:map help-map
         ("C-SPC"   . helm-swoop)
         ("M-,"     . helm-swoop-back-to-last-point)
         :map isearch-mode-map
         ("M-i"     . helm-swoop-from-isearch))
  :config
  (bind-keys :map helm-swoop-map
             ("C-r"     . helm-previous-line)
             ("C-s"     . helm-next-line)
             :map helm-multi-swoop-map
             ("C-r"     . helm-previous-line)
             ("C-s"     . helm-next-line)))
(use-package helm-xref
  :if (eq completion-framework 'helm)
  :ensure)
(use-package hl-line
  :hook ((prog-mode conf-mode compilation-mode) . hl-line-mode)
  :custom
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil))
(use-package ibuffer
  :bind ([remap list-buffers] . ibuffer)
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil))
(use-package icomplete
  :if (eq completion-framework 'icomplete)
  :hook (after-init . fido-mode))
(use-package icomplete-vertical
  :disabled
  :if (eq completion-framework 'icomplete)
  :ensure
  :hook (after-init . icomplete-vertical-mode)
  :bind (:map icomplete-minibuffer-map
              ("<down>" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
              ("<up>" . icomplete-backward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)))
(use-package ido
  :if (eq completion-framework 'ido)
  :custom
  (ido-auto-merge-work-directories-length -1)
  (ido-auto-merge-delay-time 999999)
  (ido-create-new-buffer 'always)
  (ido-enable-flex-matching t)
  (ido-enable-tramp-completion nil)
  ;; (ido-everywhere t)
  (ido-mode t)
  (ido-max-file-prompt-width 0.15)
  (ido-work-directory-match-only t)
  (ido-record-ftp-work-directories nil)
  ;; (ido-use-faces t)
  (ido-use-filename-at-point nil)
  (ido-use-url-at-point nil)
  (ido-use-virtual-buffers t)
  :config
  (ido-mode 1)
  (advice-add 'ido-switch-buffer :around
              (defun ido-switch-buffer-maybe-other-window (o &rest args)
                (if current-prefix-arg
                    (call-interactively 'ido-switch-buffer-other-window)
                  (apply o args))))
  (advice-add 'ido-init-completion-maps :after
              (defun override-next-prev-keys-with-C-n/p (&rest _)
                (bind-keys :map ido-common-completion-map
                           ("C-n" . ido-next-match)
                           ("C-p" . ido-prev-match))))
  (defun ido-fallback-to-helm-file ()
    (interactive)
    (ido-fallback-command 'helm-find-files))
  (defun ido-fallback-to-helm-buffer ()
    (interactive)
    (ido-fallback-command 'helm-buffers-list))
  (defun ido-fallback-to-ffap ()
    (interactive)
    (ido-fallback-command 'ffap))
  (bind-keys :map ido-common-completion-map
             ("M-."   . ido-fallback-to-ffap)
             :map ido-file-dir-completion-map
             ("M-P"   . ido-fallback-to-helm-file)
             :map ido-buffer-completion-map
             ("M-P"   . ido-fallback-to-helm-buffer))
  (use-package flx-ido
    :ensure
    :after ido
    :config
    (flx-ido-mode 1)))
(use-package iedit
  :ensure
  :bind (:map mode-specific-map
         ("M-RET" . iedit-mode)
         :map iedit-lib-keymap
         ("C-s" . iedit-next-occurrence)
         ("C-r" . iedit-prev-occurrence))
  :custom
  (iedit-toggle-key-default (kbd "M-RET")))
(use-package isearch
  :defer t
  :hook ((isearch-mode-end . move-to-search-start)
         (isearch-mode . search-for-region))
  :custom
  (isearch-lazy-count t)
  :config
  (defun move-to-search-start ()
    (and isearch-forward
         (number-or-marker-p isearch-other-end)
         (not mark-active)
         (not isearch-mode-end-hook-quit)
         (goto-char isearch-other-end)))
  (defun search-for-region ()
    (let ((s (get-current-active-selection)))
      (when s
        (isearch-yank-string s)
        (deactivate-mark)
        (if isearch-forward
            (isearch-repeat-forward)
          (isearch-repeat-backward))))))
(use-package ivy
  :if (eq completion-framework 'ivy)
  :ensure
  :diminish
  :hook (after-init . ivy-mode)
  :bind (:map ivy-minibuffer-map
              ("C-SPC" . ivy-toggle-mark)
              ("C-@"   . ivy-toggle-mark)
              ("TAB"   . ivy-partial)
              ("C-i"   . ivy-partial)
              ("M-'"   . ivy-avy)

              :map mode-specific-map
              ("]"   . ivy-push-view)
              ("["   . ivy-pop-view)
              ("C-r" . ivy-resume))      
  :custom
  (ivy-action-wrap nil)
  (ivy-count-format "")
  (ivy-extra-directories '("./"))
  (ivy-fixed-height-minibuffer nil)
  (ivy-height 10)
  (ivy-magic-tilde t)
  (ivy-on-del-error-function nil)
  (ivy-read-action-function 'ivy-hydra-read-action)
  (ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-wrap nil)
  (minibuffer-depth-indicate-mode t)
  :config
  (defun ivy-toggle-mark ()
    (interactive)
    (if (ivy--marked-p)
        (ivy--unmark (ivy-state-current ivy-last))
      (ivy--mark (ivy-state-current ivy-last)))
    (ivy-next-line))
  (setq search-default-mode #'char-fold-to-regexp)
  (defun ivy-switch-buffer-maybe-other-window (o &rest args)
    (if current-prefix-arg
        (let ((w (selected-window)))
          (call-interactively 'ivy-switch-buffer-other-window)
          (select-window w))
      (apply o args)))
  (advice-add 'ivy-switch-buffer :around #'ivy-switch-buffer-maybe-other-window)
  (cl-pushnew (cons 'read-file-name-internal #'ivy--regex-fuzzy)
              ivy-re-builders-alist
              :key #'car)
  (cl-pushnew (cons 'counsel-file-jump #'ivy--regex-fuzzy)
              ivy-re-builders-alist
              :key #'car)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-arrow))
(use-package ivy-hydra
  :if (eq completion-framework 'ivy)
  :ensure
  :after ivy)
(use-package counsel
  :if (eq completion-framework 'ivy)
  :ensure
  :diminish
  :hook (after-init . counsel-mode)
  :bind (([remap dired] . counsel-dired)
         ([remap tmm-menubar] . counsel-tmm)
         ([remap menu-bar-open] . counsel-tmm)
         ("M-T"   . counsel-semantic-or-imenu)
         ("M-y"   . counsel-yank-pop)

         :map counsel-find-file-map
         ("C-h"   . counsel-up-directory)
         :map ivy-minibuffer-map
         ("M-p"   . counsel-minibuffer-history)
         ("M-y"   . ivy-next-line)

         :map ctl-x-map
         ("C-r"   . counsel-recentf)
         ("`"     . counsel-compilation-errors)

         :map compilation-mode-map
         ("M-T"   . counsel-compilation-errors)

         :map help-map
         ("x"   . counsel-minor)
         ("X"   . counsel-major)
         ("r"   . counsel-register)
         ("u"   . counsel-unicode-char)
         ("SPC" . counsel-mark-ring)
         ("RET" . counsel-linux-app)

         :map mode-specific-map
         ("g"   . counsel-rg)
         ("o"   . counsel-grep-or-swiper)
         ("O"   . counsel-outline)

         :map comint-mode-map
         ("M-r" . counsel-shell-history))
  :custom
  (counsel-find-file-at-point t)
  (counsel-preselect-current-file t)
  (counsel-find-file-ignore-regexp (regexp-opt completion-ignored-extensions))
  (counsel-grep-base-command "rg -S -M 120 --no-heading --line-number --color never %s %s")
  (counsel-rg-base-command "rg -S -M 120 --no-heading --line-number --color never %s")
  (counsel-yank-pop-separator "\n----\n")
  :config
  (ivy-configure 'counsel-yank-pop
    :height ivy-height)
  (advice-add #'counsel-compilation-errors-cands :around
              (defun use-this-current-compilation-buffer-only (o &rest args)
                (if (compilation-buffer-p (current-buffer))
                    (counsel--compilation-errors-buffer (current-buffer))
                  (apply o args))))
  (defun set-initial-input-with-region (args)
    (cons (or (car args)
              (prog1 (get-current-active-selection) (deactivate-mark)))
          (cdr args)))
  (advice-add #'counsel-rg             :filter-args #'set-initial-input-with-region)
  (advice-add #'counsel-grep-or-swiper :filter-args #'set-initial-input-with-region)
  (defvar counsel-find-file-extra-actions
    '(("C-e" counsel-edit-file-name "edit file name")
      ("C-f" find-file-no-ivy "find-file")
      ("M-/" find-file-recursively "search recursively")))
  (ivy-add-actions
   'counsel-find-file
   `(,@counsel-find-file-extra-actions))
  (dolist (c (mapcar #'car counsel-find-file-extra-actions))
    (define-key counsel-find-file-map
      (kbd c) (ivy-make-magic-action 'counsel-find-file c)))
  (defun find-file-recursively (&optional initial-input)
    (interactive)
    (let ((default-directory (file-name-directory
                              (or initial-input default-directory))))
      (ivy-read "Find file: "
                (split-string (shell-command-to-string "rg --files") nil t)
                :matcher #'counsel--find-file-matcher
                :initial-input nil
                :action #'find-file
                :preselect (counsel--preselect-file)
                :require-match 'confirm-after-completion
                :history 'file-name-history
                :keymap counsel-find-file-map
                :caller 'counsel-file-jump)))
  (cl-pushnew (cons 'counsel-yank-pop 15)
              ivy-height-alist
              :key #'car)
  (cl-pushnew '("\\`file" . jump-to-register)
              counsel-register-actions
              :key #'car
              :test #'string-equal)
  (defun counsel-edit-file-name (filename)
    (interactive "s")
    (counsel-find-file (read-string "[EDIT] " filename)))
  (defun find-file-no-ivy (_)
    (interactive "s")
    (let ((completing-read-function #'completing-read-default))
      (apply #'find-file
             (find-file-read-args "Find file: "
                                  (confirm-nonexistent-file-or-buffer))))))
(use-package swiper
  :if (eq completion-framework 'ivy)
  :ensure
  :bind (:map isearch-mode-map
         ("M-s s" . swiper-isearch-toggle)
         ("M-s S" . swiper-all-from-isearch)
         :map swiper-map
         ("M-m" . swiper-mc)
         ("C-w" . ivy-yank-word)
         ("C-r" . swiper-C-r)
         ("C-s" . swiper-C-s))
  :commands swiper-mc
  :config
  (defun swiper-C-r (&optional arg)
    (interactive "p")
    (if (string= ivy-text "")
        (ivy-previous-history-element 1)
      (ivy-previous-line arg)))
  (advice-add #'swiper-isearch :filter-args #'use-current-active-selection)
  (defun use-current-active-selection (args)
    (if (car args)
        args
      (let ((s (get-current-active-selection)))
        (when s
          (deactivate-mark)
          (list s)))))
  (defun swiper-all-from-isearch ()
    (interactive)
    (let ((query (if isearch-regexp
                     isearch-string
                   (regexp-quote isearch-string))))
      (isearch-exit)
      (swiper-all query))))
(use-package ivy-rich
  :if (eq completion-framework 'ivy)
  :ensure
  :hook (after-init . ivy-rich-mode))
(use-package ivy-xref
  :if (eq completion-framework 'ivy)
  :ensure
  :functions (ivy-xref-show-xrefs ivy-xref-show-defs)
  :after xref
  :custom
  (xref-show-xrefs-function 'ivy-xref-show-xrefs)
  (xref-show-definitions-function #'ivy-xref-show-defs))
(use-package jka-cmpr-hook
  :hook (after-init . auto-compression-mode))
(use-package magit
  :ensure
  :bind (:map ctl-x-map ("g" . magit-status))
  :custom
  (magit-diff-refine-hunk t)
  :config
  (setq magit-status-buffer-switch-function 'switch-to-buffer))
(use-package mb-depth
  :hook (after-init . minibuffer-depth-indicate-mode))
(use-package multiple-cursors
  :ensure
  :bind (:map mode-specific-map
              ("e" . mc/edit-lines)
              ("A" . mc/mark-all-in-region))
  :config
  (add-to-list 'mc/cmds-to-run-once #'swiper-mc))
(use-package ob-compile
  :after org
  :bind (:map mode-specific-map ("8" . ob-compile))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((compile . t))))
(use-package org
  :bind (:map mode-specific-map
              ("l" . org-store-link)
              ("a" . org-agenda)
              ("c" . org-capture)
              :map org-mode-map
              ("C-TAB" . nil)
              ("C-c ;" . nil))
  :custom
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-odd-levels-only nil)
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  :config
  (require 'org-tempo nil t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (emacs-lisp . t) 
     (shell . t))))
(use-package osc52
  :if (getenv "TMUX")
  :hook (after-init . osc52-set-cut-function)
  :config
  (add-hook 'after-make-frame-functions (lambda (_) (osc52-set-cut-function))))
(use-package outline-magic
  :ensure
  :bind (("<backtab>" . outline-cycle)))
(use-package recentf
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup (* 3 3600))
  (recentf-max-saved-items 300))
(use-package racer
  :ensure
  :bind (:map rust-mode-map ("C-h d" . racer-describe))
  :hook (rust-mode . racer-mode))
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :custom
  (python-shell-interpreter "ipython3")
  (python-shell-interpreter-args "--simple-prompt -i"))
(use-package reveal
  :hook (after-init . global-reveal-mode))
(use-package savehist
  :hook (after-init . savehist-mode)
  :custom
  (history-delete-duplicates t)
  (savehist-additional-variables
   '(kill-ring command-history regexp-search-ring))
  :config
  (put 'savehist-minibuffer-history-variables 'history-length 50)
  (put 'org-read-date-history                 'history-length 50)
  (put 'read-expression-history               'history-length 50)
  (put 'org-table-formula-history             'history-length 50)
  (put 'extended-command-history              'history-length 50)
  (put 'ido-file-history                      'history-length 50)
  (put 'helm-M-x-input-history                'history-length 50)
  (put 'minibuffer-history                    'history-length 50)
  (put 'ido-buffer-history                    'history-length 50)
  (put 'buffer-name-history                   'history-length 50)
  (put 'file-name-history                     'history-length 50))
(use-package saveplace
  :hook (after-init . save-place-mode)
  :custom
  (save-place-forget-unreadable-files nil))
(use-package shell
  :if (eq shell-variant 'shell)
  :bind (("C-`"  . shell)
         ("C-\\" . shell)
         ("C-;"  . shell)
         :map ctl-x-map
         (","    . shell*)
         :map ctl-x-4-map
         (","    . shell-other-window*)
         :map shell-mode-map
         ([remap read-only-mode] . shell-toggle-compile-mode)
         ("M-T" . counsel-switch-to-shell-buffer)
         ("<M-right>" . next-shell-buffer)
         ("<M-left>"  . prev-shell-buffer))
  :config
  (defun shell* ()
    (interactive)
    (progn (setq unread-command-events
                 (nconc (listify-key-sequence "\exshell\015")
                        unread-command-events))
           (call-interactively 'recursive-edit*)))
  (defun shell-other-window* ()
    (interactive)
    (progn (setq unread-command-events
                 (nconc (listify-key-sequence "\eo\exshell\015")
                        unread-command-events))
           (call-interactively 'recursive-edit*)))
  (defun shell-toggle-compile-mode ()
    (interactive)
    (setq buffer-read-only t)
    (compilation-mode))
  (defun next-shell-buffer (arg)
    (interactive "p")
    (let* ((shell-buffer-p (lambda (b)
                             (eq 'shell-mode
                                 (buffer-local-value 'major-mode b))))
           (by-buffer-name (lambda (a b)
                             (string< (buffer-name a) (buffer-name b))))
           (shell-buffers
            (sort (cl-remove-if-not shell-buffer-p (buffer-list)) by-buffer-name))
           (pos (cl-position (current-buffer) shell-buffers)))
      (when pos
        (set-window-buffer
         (selected-window)
         (nth (% (+ arg pos (length shell-buffers))
                 (length shell-buffers))
              shell-buffers)))))
  (defun prev-shell-buffer (arg)
    (interactive "p")
    (next-shell-buffer (- arg)))
  (advice-add 'shell :around 'shell-dwim)
  (defun shell-dwim (o &rest args)
    (if (not (and (eq major-mode 'shell-mode)
                  (called-interactively-p 'interactive)
                  (null current-prefix-arg)))
        (let ((d default-directory)
              (b (apply o args)))
          (unless (process-running-child-p (get-buffer-process b))
            (with-current-buffer b
              (insert "cd " d)
              (comint-send-input))))
      (bury-buffer)
      (unless (one-window-p 'nomini)
        (delete-window)
        (other-window -1))))
  (defun use-region-if-active (o &rest args)
    (let ((s (and (memq this-command '(compile shell-command async-shell-command))
                  (get-current-active-selection))))
      (if (null s)
          (apply o args)
        (let (r)
          (dolist (l (split-string s "\n"))
            (push (if (string-match (concat "^ *" comment-start "*[ \t]*") l)
                      (replace-match "" nil t l 0)
                    l)
                  r))
          (mapconcat #'identity (nreverse r) "\n")))))
  (advice-add 'read-shell-command :around #'use-region-if-active))
(use-package smerge-mode
  :custom
  (smerge-command-prefix "\C-z"))
(use-package smex
  :ensure
  :hook (after-init . smex-initialize))
(eval-and-compile
  (defmacro use-package-when (pkg cond &rest body)
    `(use-package ,pkg
       ,@(if (eval cond)
             body
           (cons ':disabled body)))))
(use-package-when tab-bar (>= (string-to-number emacs-version) 27)
  :bind (("<C-prior>" . tab-previous)
         ("<C-next>"  . tab-next)
         :map tab-prefix-map
         ("O" . tab-previous)
         ("t" . tab-switcher))
  :custom
  (tab-bar-show nil))
(use-package undo-tree
  :ensure
  :diminish
  :custom
  (undo-tree-mode-lighter "")
  :hook (after-init . global-undo-tree-mode))
(use-package uniquify
  :defer t
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'reverse)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-separator "|"))
(use-package vlf
  :ensure
  :defer t
  :config
  (require 'vlf-setup))
(use-package volatile-highlights
  :ensure
  :diminish
  :hook (after-init . volatile-highlights-mode))
(use-package wgrep
  :ensure
  :hook (grep-setup . wgrep-setup)
  :custom
  (wgrep-enable-key "\C-x\C-q"))
(use-package wrap-region
  :ensure
  :diminish
  :hook ((after-init . wrap-region-global-mode)
         (wrap-region-after-wrap . move-to-wrapped-region))
  :config
  (defun move-to-wrapped-region ()
    (when (and (boundp 'left) (equal (string (char-after)) left))
      (forward-char 1))))
(use-package windresize
  :ensure
  :bind (:map mode-specific-map ("w" . windresize)))
(use-package xref
  :bind (("<f3>" . xref-find-definitions)
         ("<f4>" . xref-find-references))
  :custom
  (xref-after-jump-hook nil)
  (xref-after-return-hook nil)
  (xref-prompt-for-identifier '(not xref-find-definitions
                                    xref-find-definitions-other-window
                                    xref-find-definitions-other-frame
                                    xref-find-references)))
(use-package xterm-color
  :ensure
  :after shell
  :hook (shell-mode . setup-color-for-shell)
  :config
  (defun setup-color-for-shell ()
    (font-lock-mode -1)
    (setq-local font-lock-function (lambda (_) nil))
    (when (require 'xterm-color nil t)
      (setq comint-output-filter-functions
            (remove 'ansi-color-process-output comint-output-filter-functions))
      (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))))

(if use-dark-mode
    (progn (custom-set-variables
            '(frame-background-mode 'dark))
           (custom-set-faces
            '(header-line        ((t (:inherit mode-line))))
            '(mode-line          ((t (:foreground "#3f3f3f" :background "#dcdccc" :box nil))))
            '(mode-line-inactive ((t (:foreground "grey20" :background "grey30" :box nil))))
            '(region             ((t (:background "#6f6f6f"))))
            '(vertical-border    ((t (:foreground "grey30" :background "grey30" :box nil))))
            '(hl-line                  ((t (:background "#4f4f4f"))))
            '(line-number-current-line ((t (:inherit hl-line))))
            '(diff-added                  ((t :foreground "green"  :background "gray10")))
            '(diff-removed                ((t :foreground "yellow" :background "gray10")))
            '(ediff-current-diff-A        ((t :background "gray20" :inherit diff-added)))
            '(ediff-current-diff-B        ((t :background "gray20" :inherit diff-added)))
            '(ediff-current-diff-C        ((t :background "gray20" :inherit diff-added)))
            '(ediff-current-diff-Ancestor ((t :background "gray20" :inherit diff-added)))
            '(ediff-fine-diff-A           ((t :background "gray30" :inherit diff-added)))
            '(ediff-fine-diff-B           ((t :background "gray30" :inherit diff-added)))
            '(ediff-fine-diff-C           ((t :background "gray30" :inherit diff-added)))
            '(ediff-fine-diff-Ancestor    ((t :background "gray30" :inherit diff-added)))
            '(ediff-even-diff-A           ((t :background "gray10")))
            '(ediff-even-diff-B           ((t :background "gray10")))
            '(ediff-even-diff-C           ((t :background "gray10")))
            '(ediff-even-diff-Ancestor    ((t :background "gray10")))
            '(ediff-odd-diff-A            ((t :background "gray15")))
            '(ediff-odd-diff-B            ((t :background "gray15")))
            '(ediff-odd-diff-C            ((t :background "gray15")))
            '(ediff-odd-diff-Ancestor     ((t :background "gray15")))
            '(diff-refine-added           ((t :background "gray30" :bold t :inherit diff-added)))
            '(diff-refine-change          ((t :background "gray30" :bold t :inherit diff-changed)))
            '(diff-refine-removed         ((t :background "gray30" :bold t :inherit diff-removed)))
            '(ivy-current-match ((t (:inherit secondary-selection))))
            '(helm-ff-prefix                   ((t :inherit dired-header)))
            '(helm-ff-executable               ((t :inherit dired-perm-write)))
            '(helm-ff-suid                     ((t :inherit dired-set-id)))
            '(helm-ff-directory                ((t :inherit dired-directory)))
            '(helm-ff-dotted-directory         ((t :inherit helm-ff-directory)))
            '(helm-ff-dotted-symlink-directory ((t :inherit helm-ff-symlink)))
            '(helm-ff-symlink                  ((t :inherit dired-symlink)))
            '(helm-ff-invalid-symlink          ((t :inherit error)))
            '(helm-ff-denied                   ((t :inherit error)))
            '(helm-ff-file                     ((t :inherit default)))
            '(helm-ff-truename                 ((t :inherit dired-flagged)))
            '(helm-ff-dirs                     ((t :inherit dired-directory)))
            '(helm-ff-socket                   ((t :inherit dired-special)))
            '(helm-ff-pipe                     ((t :inherit dired-special)))
            '(helm-ff-file-extension           ((t :inherit font-lock-type-face)))
            '(helm-ff-backup-file              ((t :inherit dired-ignored)))))
  (custom-set-variables
   '(frame-background-mode 'light))
  (custom-set-faces
   '(header-line        ((t (:inherit mode-line))))
   '(mode-line          ((t (:background "grey75" :foreground "black" :box nil))))
   '(mode-line-inactive ((t (:foreground "grey20" :background "grey90" :box nil))))
   '(vertical-border    ((t (:foreground "grey30" :background "grey90" :box nil))))
   '(line-number-current-line ((t (:inherit hl-line))))))


;; end of .emacs.el
