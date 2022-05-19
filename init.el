;; -*- lexical-binding: t -*-
;; jay+nospam@kldp_remove_me_.org
(setq custom-file "/dev/null")
(advice-add #'custom-save-all :override #'ignore)
(add-to-list 'load-path "~/.emacs.d/lisp")

(unless (display-graphic-p)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (defun restore-tty-run-terminal-initialization ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))
(custom-set-variables
 '(ad-redefinition-action 'accept)
 '(async-shell-command-buffer 'rename-buffer)
 '(async-shell-command-display-buffer nil)
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(auto-window-vscroll nil)
 '(backup-by-copying t)
 '(backup-by-copying-when-linked t)
 '(backup-by-copying-when-mismatch t)
 '(backup-directory-alist '(("." . "~/.cache/emacs/backups")))
 '(bidi-display-reordering 'left-to-right)
 '(bidi-inhibit-bpa t)
 '(bidi-paragraph-direction 'left-to-right)
 '(blink-matching-paren t)
 '(calc-display-trail nil)
 '(column-number-indicator-zero-based nil)
 '(completion-auto-help 'lazy)
 '(completion-category-defaults nil)
 '(completion-category-overrides '((file (styles . (partial-completion)))))
 '(completion-cycle-threshold 1)
 '(completion-ignore-case t)
 '(completion-pcm-complete-word-inserts-delimiters t)
 '(completion-show-help nil)
 '(confirm-kill-emacs nil)
 '(confirm-nonexistent-file-or-buffer nil)
 '(create-lockfiles nil)
 '(cursor-in-non-selected-windows nil)
 '(delete-old-versions t)
 '(disabled-command-function nil)
 '(display-buffer-alist '(("\\*shell\\*" display-buffer-same-window)))
 '(enable-recursive-minibuffers t)
 '(eval-expression-print-length nil)
 '(eval-expression-print-level nil)
 '(even-window-heights nil)
 '(fast-but-imprecise-scrolling t)
 '(ffap-machine-p-known 'reject)
 '(fit-window-to-buffer-horizontally t)
 '(font-lock-maximum-decoration '((c-mode . 2) (c++-mode . 2) (t . t)))
 '(frame-inhibit-implied-resize t)
 '(frame-resize-pixelwise t)
 '(hscroll-margin 2)
 '(hscroll-step 1)
 '(help-char ?^)
 '(history-delete-duplicates t)
 '(history-length 1000)
 '(highlight-nonselected-windows nil)
 '(find-file-visit-truename nil)
 '(idle-update-delay 1)
 '(indent-tabs-mode nil)
 '(inhibit-compacting-font-caches t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kill-buffer-query-functions nil)
 '(kill-do-not-save-duplicates t)
 '(kill-read-only-ok t)
 '(kill-ring-max 3000)
 '(kill-whole-line t)
 '(line-move-visual nil)
 '(mac-option-key-is-meta t)
 '(mac-right-option-modifier nil)
 '(make-backup-files nil)
 '(mark-even-if-inactive t)
 '(max-mini-window-height 0.15)
 '(minibuffer-eldef-shorten-default t)
 '(minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
 '(mode-line-client '(""))
 '(mode-line-modified '("%* "))
 '(mode-line-remote '(""))
 '(mode-line-frame-identification '(""))
 '(mode-line-front-space '("  "))
 '(mode-line-position '(""))
 '(mode-line-mule-info '(""))
 '(mode-line-end-spaces
   '(:eval (concat (propertize " " 'display `(space :align-to (- right 15)))
                   (let* ((tabs (frame-parameter nil 'tabs))
                          (current (cdr (assq 'name (and (> (length tabs) 1) (assq 'current-tab tabs))))))
                     (if current
                         (concat "[" current "]")
                       ""))
                   (propertize " " 'display `(space :align-to (- right 6)))
                   "%l:%c")))
 '(next-error-message-highlight t)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(ns-tool-bar-display-mode 'both)
 '(ns-tool-bar-size-mode 'regular)
 '(ns-use-thin-smoothing t)
 '(read-buffer-completion-ignore-case t)
 '(read-extended-command-predicate #'command-completion-default-include-p)
 '(read-file-name-completion-ignore-case t)
 '(read-process-output-max (* 1024 1024))
 '(redisplay-skip-fontification-on-input t)
 '(require-final-newline t)
 '(resize-mini-windows 'grow-only)
 '(revert-without-query '(""))
 '(ring-bell-function 'ignore)
 '(save-interprogram-paste-before-kill t)
 '(scroll-conservatively 101)
 '(scroll-preserve-screen-position t)
 '(scroll-margin 0)
 '(scroll-step 1)
 '(select-active-regions nil)
 '(server-client-instructions nil)
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(shell-command-switch "-lc")
 '(shell-command-default-error-buffer "*Shell Command Errors*")
 '(show-trailing-whitespace nil)
 '(split-height-threshold nil)
 '(split-width-threshold 160)
 '(suggest-key-bindings nil)
 '(tab-always-indent 'complete)
 '(truncate-lines t)
 '(use-dialog-box nil)
 '(use-package-compute-statistics nil)
 '(use-package-enable-imenu-support t)
 '(use-short-answers t)
 '(vc-follow-symlinks nil)
 '(version-control t)
 '(view-read-only t)
 '(visible-bell nil)
 '(x-underline-at-descent-line t)
 '(x-selection-timeout 100)
 '(y-or-n-p-use-read-key t)
 '(warning-minimum-level :emergency)
 '(window-divider-default-places 'right-only)
 '(window-divider-default-bottom-width 1)
 '(window-divider-default-right-width 1)
 '(window-resize-pixelwise nil)
 '(words-include-escapes t))

(put 'backup-inhibited 'safe-local-variable 'booleanp)
(defvar set-mark-dwim-timeout 0.5)
(defvar set-mark-dwim-repeat-action 'embark-act)
(defvar set-mark-dwim-timeout-action 'completion-at-point)
(defun set-mark-dwim (o &rest args)
  (cond ((or (not (called-interactively-p 'interactive))
             current-prefix-arg
             (memq last-command '(pop-to-mark-command pop-global-mark)))
         (apply o args))
        ((not (sit-for set-mark-dwim-timeout))
         (let ((cmd (lookup-key (current-active-maps) (read-key-sequence ""))))
           (if (memq cmd '(set-mark-command cua-set-mark))
               (call-interactively set-mark-dwim-repeat-action)
             (apply o args)
             (call-interactively cmd))))
        (t
         (call-interactively set-mark-dwim-timeout-action))))
(advice-add #'set-mark-command :around #'set-mark-dwim)
(advice-add #'display-startup-echo-area-message :override #'ignore)
(advice-add #'goto-line-read-args :around
            (defun display-line-numbers (o &rest args)
              (let ((display-line-numbers t))
                (apply o args))))
(dotimes (i 10)
  (define-key goto-map (number-to-string i) #'dispatch-goto-line))
(defun dispatch-goto-line ()
  (interactive)
  (setq unread-command-events (nconc (list (logand ?\xff last-command-event)) unread-command-events))
  (call-interactively 'goto-line))
(advice-add #'recursive-edit :around
            (defun preseve-window-configuration-if-interactive (o)
              (let ((wc (and (called-interactively-p 'interactive)
                             (current-window-configuration))))
                (unwind-protect (funcall o)
                  (and wc (set-window-configuration wc))))))
(advice-add 'split-window-right :after #'call-other-window-if-interactive)
(advice-add 'split-window-below :after #'call-other-window-if-interactive)
(defun call-other-window-if-interactive (&rest _)
  (when (called-interactively-p 'interactive)
    (other-window 1)))
(advice-add 'other-window :around
            (defun switch-to-last-buffer-if-one-window (o &rest args)
              (if (and (one-window-p 'nomini) (called-interactively-p 'interactive))
                  (switch-to-buffer nil)
                (apply o args))))

(defun delete-other-window ()
  (interactive)
  (unless (one-window-p 'nomini)
    (other-window 1)
    (if current-prefix-arg
        (progn (bury-buffer) (other-window -1))
      (delete-window (selected-window)))))
(defun get-current-active-selection ()
  (let ((p (if (use-region-p)
               (cons (region-beginning) (region-end))
             (and (fboundp 'easy-kill--bounds)
                  (ignore-errors (funcall 'easy-kill--bounds))))))
    (and p (car p) (buffer-substring-no-properties (car p) (cdr p)))))
(add-hook #'after-init-hook #'delete-selection-mode)
(add-hook #'after-init-hook #'find-function-setup-keys)
(add-hook #'after-init-hook #'minibuffer-depth-indicate-mode)
(add-hook #'after-init-hook #'minibuffer-electric-default-mode)
(add-hook #'after-init-hook #'repeat-mode)
(add-hook #'after-init-hook #'window-divider-mode)
(add-hook #'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

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
  (dolist (p '(use-package diminish bind-key))
    (unless (require p nil t)
      (package-refresh-contents)
      (package-install p))))

(eval-when-compile
  (require 'use-package nil t)
  (require 'bind-key nil t))

(eval-and-compile
  (defmacro csetq (sym val)
    `(funcall (or (get ',sym 'custom-set) 'set-default) ',sym ,val)))
(use-package diminish
  :ensure
  :config
  (diminish 'abbrev-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'flymake-mode))
(use-package ace-window
  :ensure
  :bind ("C-x o" . ace-window))
(use-package amx
  :ensure
  :hook (after-init . amx-mode))
(use-package auto-highlight-symbol
  :ensure
  :diminish
  :hook (after-init . global-auto-highlight-symbol-mode)
  :bind ( :map auto-highlight-symbol-mode-map
          ("C-x '" . ahs-change-range)))
(use-package avy
  :ensure
  :bind (("C-'"       . avy-goto-char-timer)
         ("C-c C-SPC" . avy-goto-char-timer))
  :config
  (avy-setup-default)
  (advice-add 'avy-goto-char-timer :around
              (defun avy-pop-mark-if-prefix (o &rest args)
                (if current-prefix-arg
                    (call-interactively (if (eq 4 (car current-prefix-arg))
                                            'avy-pop-mark
                                          'avy-resume))
                  (apply o args))))
  (setf (alist-get ?. avy-dispatch-alist)
        (defun avy-action-embark (pt)
          (unwind-protect
              (save-excursion
                (goto-char pt)
                (embark-act))
            (select-window
             (cdr (ring-ref avy-ring 0))))
          t))
  (setf (alist-get ?  avy-dispatch-alist)
        (defun avy-action-mark-to-char (pt)
          (activate-mark)
          (goto-char pt))))
(use-package bash-completion
  :ensure
  :hook (after-init . bash-completion-setup)
  :custom
  (bash-completion-use-separate-processes t))
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
  :bind ("M-I" . ffap-include-file)
  :hook (c-mode-common . set-outline-regexp)
  :config
  (defun ffap-include-file ()
    (interactive)
    (let (files)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^#include\\s-*[\"<]\\([^\">]+\\)[\">]\\s-*$" nil t)
          (push (match-string 1) files))
        (setq files (nreverse files)))
      (find-file (save-excursion
                   (let ((match (completing-read "Open include: " files)))
                     (goto-char (point-min))
                     (re-search-forward (concat "^#include\\s-*[\"<]" (regexp-quote match) "[\">]\\s-*$"))
                     (re-search-backward "[\">]")
                     (ffap-guesser))))))
  (advice-add #'ffap-c++-mode :around
              (defun search-within-project (o name)
                (let ((r (funcall o name)))
                  (unless r
                    (when-let (d (locate-dominating-file "." name))
                      (setq r (concat d name))))
                  r)))
  (defun set-outline-regexp ()
    (require 'outline)
    (setq outline-regexp "\\s-*\\S-"))
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
  (advice-add 'c-outline-level :around #'merge-empty-lines-with-prevous-line)
  (advice-add #'c-indent-line-or-region :around #'indent-dwim)
  (defun indent-dwim (o &optional arg region)
    (if region
        (funcall o arg region)
      (let ((old-tick (buffer-chars-modified-tick))
            (old-point (point))
            (syn `(,(syntax-after (point)))))
        (funcall o arg region)
        (when (and (eq tab-always-indent 'complete)
                   (eq old-point (point))
                   (eq old-tick (buffer-chars-modified-tick))
                   (or (null tab-first-completion)
                       (eq last-command this-command)
                       (and (equal tab-first-completion 'eol)
                            (eolp))
                       (and (member tab-first-completion
                                    '(word word-or-paren word-or-paren-or-punct))
                            (not (member 2 syn)))
                       (and (member tab-first-completion
                                    '(word-or-paren word-or-paren-or-punct))
                            (not (or (member 4 syn)
                                     (member 5 syn))))
                       (and (equal tab-first-completion 'word-or-paren-or-punct)
                            (not (member 1 syn)))))
          (completion-at-point))))))
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
  ;; (comint-prompt-read-only t)
  (comint-scroll-to-bottom-on-input nil)
  ;; (comint-use-prompt-regexp t)
  :config
  ;; (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
  ;; (add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
  (defun do-silently (o &rest args)
    (let ((inhibit-message t))
      (apply o args)))
  (advice-add 'comint-previous-matching-input :around #'do-silently))
(use-package company
  :ensure
  :diminish
  :hook (after-init . global-company-mode)
  :bind ( :map prog-mode-map
          ("C-i"   . company-indent-or-complete-common)
          :map mode-specific-map
          ("/" . company-manual-begin)
          :map company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("M-s" . company-filter-candidates))
  :custom
  ;; (company-auto-complete t)
  (company-idle-delay nil)
  (company-tooltip-idle-delay nil)
  (company-tooltip-align-annotations t))
(use-package compile
  :diminish compilation-in-progress
  :hook ((compilation-mode . run-before-compile)
         (compilation-filter . apply-xterm-color-filter))
  :bind (("<f7>" . compile)
         ("<f8>" . recompile)
         :map compilation-mode-map
         ("M-{" . nil)
         ("M-}" . nil)
         ("." . rename-uniquely)
         ("t" . toggle-truncate-lines)
         ([remap read-only-mode] . compilation-toggle-shell-mode))
  :custom
  (compilation-environment '("TERM=xterm-256color"))
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-save-buffers-predicate (lambda ()))
  (compilation-scroll-output 'first-error)
  :config
  (defun apply-xterm-color-filter ()
    (let* ((proc (get-buffer-process (current-buffer)))
           (end-marker (and proc (process-mark proc)))
           (buffer-undo-list t))
      ;; (let ((s (buffer-substring-no-properties compilation-filter-start end-marker)))
      ;;   (unless (string= s "\033[J")
      ;;     (with-current-buffer (get-buffer-create "*OUTPUT*")
      ;;       (insert "\n" s "\n"))))
      (save-excursion
        (goto-char compilation-filter-start)
        (while (re-search-forward (rx "\033[" (group (*? num)) (group (any "GAJK"))) end-marker t)
          (let ((count (and (> (length (match-string 1)) 0) (string-to-number (match-string 1))))
                (c (aref (match-string 2) 0)))
            (cond ((= c ?G)
                   (delete-region (point-at-bol) (point)))
                  ((= c ?A)
                   (delete-region (point-at-bol (- (1- (or count 1)))) (point)))
                  ((= c ?K)
                   ;; 0 (or missing) -> point to eol
                   ;; 1 -> bol to point
                   ;; 2 -> bol to eol
                   (unless (= 0 (or count 0))
                     (delete-region (point-at-bol) (point))))
                  ((= c ?J)
                   ;; 0 (or missing) -> point to end of screen
                   ;; 1 -> beginning of screen to point
                   ;; 2 -> entire screen
                   ;; 3 -> entire screen & all lines in scollback buffer
                   (replace-match "")))
            (setq compilation-filter-start (min (point) compilation-filter-start))))
        (goto-char end-marker)
        (let* ((s (buffer-substring-no-properties compilation-filter-start end-marker))
               (ns (ansi-color-apply (xterm-color-filter s))))
          (unless (string-equal s ns)
            (delete-region compilation-filter-start end-marker)
            (insert ns)))
        (set-marker end-marker (point)))))
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
(use-package compiler-explorer
  :ensure
  :bind (:map help-map ("C" . compiler-explorer)))
(use-package consult
  :ensure
  :bind (("M-\"" . consult-register-load)
         ("M-'"  . consult-register-store)
         ([remap yank-pop] . consult-yank-pop)
         ("M-T" . consult-imenu)

         :map help-map
         ("a"     . consult-apropos)
         ("C-m"   . consult-man)
         ("SPC"   . consult-mark)
         ("C-SPC" . consult-global-mark)
         ("x"     . consult-minor-mode-menu)
         ("X"     . consult-mode-command)

         :map minibuffer-local-map
         ("C-r"   . consult-history)

         :map mode-specific-map
         ("h"   . consult-history)
         ("b"   . consult-bookmark)
         ("g"   . consult-ripgrep)
         ("C-g" . consult-grep)
         ("G"   . consult-git-grep)
         ("k"   . consult-kmacro)
         ("m"   . consult-mode-command)

         :map ctl-x-map
         ("M-:" . consult-complex-command)
         ("b"   . consult-buffer)
         ("C-r" . consult-recent-file)
         ("`"   . consult-compile-error)

         :map ctl-x-4-map
         ("b" . consult-buffer-other-window)

         :map ctl-x-5-map
         ("b" . consult-buffer-other-frame)

         :map goto-map
         ("g" . consult-goto-line)
         ("M-g" . consult-goto-line)
         ("o" . consult-outline)
         ("I" . consult-imenu-multi)
         ("e" . consult-compile-error)
         :map search-map
         ("l" . consult-line)
         ("L" . consult-line-multi)
         ("m" . consult-multi-occur)
         ("o" . consult-line-symbol-at-point)
         ("O" . consult-focus-lines-symbol-at-point)
         ("k" . consult-keep-lines)
         ("u" . consult-focus-lines)
         ("e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-l" . consult-line)
         ("M-L" . consult-line-multi))
  :custom
  (register-preview-delay 0)
  (register-preview-function #'consult-register-format)
  ;; (consult-find-command "fd --color=never --full-path ARG OPTS")
  (consult-preview-key 'any)
  (consult-narrow-key "<")
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (advice-add 'kill-line :around #'consult-kill-line-dwim)
  (defun consult-kill-line-dwim (o &rest args)
    (require 'embark nil t)
    (let* ((target (and (minibufferp) (car (embark--targets))))
           (type (plist-get target :type)))
      (cond ((eq 'buffer type)
             (kill-buffer (plist-get target :target)))
            ((eq 'file type)
             (delete-file (plist-get target :target)))
            (t
             (apply o args)))))
  (advice-add #'substitute-in-file-name :around
              (defun keep-url (o arg)
                (if (string-match "^https?://" arg)
                    arg
                  (funcall o arg))))
  (advice-add #'find-file-read-args :filter-return
              (defun may-browse-url (r)
                (if (string-match "^https?://" (car r))
                    (progn
                      (browse-url (car r))
                      (exit-minibuffer))
                  r)))
  (advice-add #'completing-read-multiple :override
              #'consult-completing-read-multiple)
  ;; (nconc consult--source-bookmark (list :state #'consult--bookmark-preview))
  ;; (nconc consult--source-file (list :state #'consult--file-preview))
  ;; (nconc consult--source-project-file (list :state #'consult--file-preview))
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'consult-imenu :around
              (defun consult-imenu-across-all-buffers (o &rest args)
                (if current-prefix-arg
                    (call-interactively 'consult-imenu-multi)
                  (apply o args))))
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (defun consult-focus-lines-symbol-at-point ()
    (interactive)
    (consult-focus-lines
     nil
     (lambda (pattern cands)
       (consult--completion-filter
        pattern cands 'consult-location 'highlight))
     (thing-at-point 'symbol)))
  (defvar-local consult-toggle-preview-orig nil)
  (defun consult-toggle-preview ()
    "Command to enable/disable preview."
    (interactive)
    (if consult-toggle-preview-orig
        (setq consult--preview-function consult-toggle-preview-orig
              consult-toggle-preview-orig nil)
      (setq consult-toggle-preview-orig consult--preview-function
            consult--preview-function #'ignore)))
  (eval-after-load "vertico"
    '(define-key vertico-map (kbd "M-P") #'consult-toggle-preview))
  (defun consult-buffer-state-no-tramp ()
    "Buffer state function that doesn't preview Tramp buffers."
    (let ((orig-state (consult--buffer-state))
          (filter (lambda (action cand)
                    (if (or action
                            (let ((buffer (get-buffer cand)))
                              (and buffer
                                   (not (file-remote-p (buffer-local-value 'default-directory buffer))))))
                        cand
                      nil))))
      (lambda (action cand)
        (funcall orig-state action (funcall filter action cand)))))
  (setq consult--source-buffer
        (plist-put consult--source-buffer :state #'consult-buffer-state-no-tramp)))
(use-package consult-flycheck
  :ensure
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))
(use-package corfu
  :ensure
  ;; :custom
  ;; (corfu-auto t)
  ;; (corfu-quit-at-boundary t)
  ;; (corfu-quit-no-match t)
  :hook (after-init . global-corfu-mode))
(use-package coterm
  :ensure
  :bind (:map comint-mode-map
              ("C-;" . coterm-char-mode-cycle)
              ("C-c j" . coterm-char-mode-cycle))
  :hook (after-init . coterm-mode))
(use-package dabbrev
  :bind (("C-M-_" . dabbrev-completion)
         ("C-M-/" . dabbrev-completion))
  :custom
  (abbrev-suggest t)
  (dabbrev-case-fold-search nil)
  :config
  (advice-add 'dabbrev--find-expansion :around
              (defun suppress-message (o &rest args)
                (let ((inhibit-message t))
                  (apply o args)))))
(use-package diffview
  :ensure
  :after diff-mode
  :bind (:map diff-mode-map
              ("|" . diffview-current)))
(use-package dired
  :bind ( :map dired-mode-map
          ("^" . dired-up-directory-inplace)
          ([remap dired-do-find-regexp] . dired-do-multi-occur))
  :custom
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-no-confirm t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-use-ls-dired nil)
  :config
  (defun dired-up-directory-inplace ()
    (interactive)
    (find-alternate-file ".."))
  (defun dired-do-multi-occur (regexp)
    "Run `dired-do-multi-occur` with REGEXP on all marked files."
    (interactive (list (read-regexp "Regexp: ")))
    (multi-occur (mapcar 'find-file-noselect (dired-get-marked-files)) regexp))
  (advice-add #'dired-find-file-other-window :around
              (defun force-horizontal-split (o &rest args)
                (let ((split-width-threshold (frame-width)))
                  (apply o args)))))
(use-package dired-subtree
  :ensure
  :after dired
  :bind (:map dired-mode-map ("TAB" . dired-subtree-toggle))
  :custom-face
  (dired-subtree-depth-1-face ((t :inherit default)))
  (dired-subtree-depth-2-face ((t :inherit default)))
  (dired-subtree-depth-3-face ((t :inherit default)))
  (dired-subtree-depth-4-face ((t :inherit default)))
  (dired-subtree-depth-5-face ((t :inherit default)))
  (dired-subtree-depth-6-face ((t :inherit default))))
(use-package dired-x
  :after dired
  :hook (dired-mode . dired-extra-startup))
(use-package easy-kill
  :ensure
  :bind (([remap kill-ring-save]              . easy-kill)
         :map easy-kill-base-map
         ([remap exchange-point-and-mark]     . easy-kill-exchange-point-and-mark)
         ([remap set-mark]                    . easy-kill-mark-region)
         ([remap cua-exchange-point-and-mark] . easy-kill-exchange-point-and-mark)
         ([remap cua-set-mark]                . easy-kill-mark-region)
         ("o" . easy-kill-expand)
         ("i" . easy-kill-shrink)))
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
(use-package elec-pair
  :hook (after-init . electric-pair-mode))
(use-package elfeed
  :bind ("C-x !" . elfeed))
(use-package embark
  :ensure
  :commands (embark-act embark-prefix-help-command)
  :bind (("M-SPC" . embark-act)
         ("M-."   . embark-dwim)
         :map minibuffer-local-map
         ("M-E"   . embark-export))
  :custom
  (embark-cycle-key (kbd "M-SPC"))
  (prefix-help-command #'embark-prefix-help-command)
  (embark-help-key "?")
  (embark-indicators '(embark--vertico-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-quit-after-action nil)
  :config
  ;; https://github.com/oantolin/embark/issues/464
  (push 'embark--ignore-target
        (alist-get 'xref-find-definitions embark-target-injection-hooks))
  (push 'embark--ignore-target
        (alist-get 'xref-find-references embark-target-injection-hooks))
  (push #'embark--xref-push-marker
        (alist-get 'find-file embark-pre-action-hooks))
  (eval-after-load "which-key"
    (progn
      (add-to-list 'embark-indicators 'embark-which-key-indicator)
      (defun embark-which-key-indicator ()
        "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
        (lambda (&optional keymap targets prefix)
          (if (null keymap)
              (which-key--hide-popup-ignore-command)
            (which-key--show-keymap
             (if (eq (plist-get (car targets) :type) 'embark-become)
                 "Become"
               (format "Act on %s '%s'%s"
                       (plist-get (car targets) :type)
                       (embark--truncate-target (plist-get (car targets) :target))
                       (if (cdr targets) "…" "")))
             (if prefix
                 (pcase (lookup-key keymap prefix 'accept-default)
                   ((and (pred keymapp) km) km)
                   (_ (key-binding prefix 'accept-default)))
               keymap)
             nil nil t (lambda (binding)
                         (not (string-suffix-p "-argument" (cdr binding))))))))

      (defun embark-hide-which-key-indicator (fn &rest args)
        "Hide the which-key indicator immediately when using the completing-read prompter."
        (which-key--hide-popup-ignore-command)
        (let ((embark-indicators
               (remq #'embark-which-key-indicator embark-indicators)))
          (apply fn args)))
      (advice-add #'embark-completing-read-prompter
                  :around #'embark-hide-which-key-indicator))))
(use-package embark-consult
  :ensure
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))
(use-package eshell
  :hook (eshell-mode . setup-color-for-eshell)
  ;; :bind ("C-`" . eshell)
  :config
  (defun setup-color-for-eshell ()
    (setenv "TERM" "xterm-256color")
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions))))
(use-package esh-autosuggest
  :ensure
  :hook (eshell-mode . esh-autosuggest-mode))
(use-package eshell-syntax-highlighting
  :ensure
  :after eshell-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure
  :hook (after-init . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments '("-l"))
  :config
  (dolist (env (split-string (shell-command-to-string "bash -lc env") "\n" t))
    (let* ((p (split-string env "=" nil))
           (name (car p))
           (values (cdr p)))
      (unless (member name exec-path-from-shell-variables)
        (setenv name (mapconcat #'identity values "="))))))
(use-package gcmh
  :ensure
  :diminish
  :hook (after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold (* 16 1024 1024)))
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
  (when nil
    (async-shell-command
     (concat (mapconcat
              (lambda (e) (concat "go install " (cdr e) "@latest"))
              '((gocode    . "github.com/mdempsky/gocode")
                (golint    . "golang.org/x/lint/golint")
                (godef     . "github.com/rogpeppe/godef")
                (errcheck  . "github.com/kisielk/errcheck")
                (godoc     . "golang.org/x/tools/cmd/godoc")
                (gogetdoc  . "github.com/zmb3/gogetdoc")
                (gopls     . "golang.org/x/tools/gopls")
                (gotools   . "golang.org/x/tools/cmd/..."))
              "\n")
             "\n")))
  (add-to-list 'exec-path (expand-file-name "~/go/bin"))
  (defun go-mode-setup ()
    (add-hook 'before-save-hook 'gofmt-before-save)
    (unless (string-match "go" compile-command)
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))))
(use-package goto-last-change
  :ensure
  :bind ("M-g M-/" . goto-last-change))
(use-package hermes
  :load-path "~/work/hermes"
  :bind (:map mode-specific-map ("v" . hermes))
  :config
  (add-to-list 'display-buffer-alist '("\\*hermes.*" display-buffer-same-window)))
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
  :hook (isearch-mode . search-for-region)
  :custom
  (isearch-allow-scroll t)
  (isearch-lazy-count t)
  (lazy-highlight-buffer t)
  :config
  (defun search-for-region ()
    (let ((s (get-current-active-selection)))
      (when s
        (isearch-yank-string s)
        (deactivate-mark)
        (if isearch-forward
            (isearch-repeat-forward)
          (isearch-repeat-backward))))))
(use-package jka-cmpr-hook
  :hook (after-init . auto-compression-mode))
(use-package magit
  :ensure
  :bind (:map ctl-x-map ("g" . magit-status))
  :custom
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function 'magit-display-buffer-same-window)
  :config
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (defun magit-display-buffer-same-window (buffer)
    (display-buffer buffer '(display-buffer-same-window))))
(use-package marginalia
  :ensure
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators
   '(marginalia-annotators-light marginalia-annotators-heavy))
  :hook (after-init . marginalia-mode)
  :config
  (advice-add #'marginalia--buffer-file :around
              (lambda (o &rest args)
                ;; marginalia--buffer-file calls abbreviate-file-name which is very slow for remote path.
                (let ((original (symbol-function #'abbreviate-file-name)))
                  (cl-letf (((symbol-function #'abbreviate-file-name)
                             (lambda (filename)
                               (if (file-remote-p filename)
                                   filename
                                 (funcall original filename)))))
                    (apply o args))))))
(use-package modus-themes
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-hl-line '(accented intense))
  (modus-themes-italic-constructs t)
  (modus-themes-mode-line '(moody accented borderless))
  (modus-themes-region '(bg-only))
  (modus-themes-scale-headings t)
  (modus-themes-slanted-constructs t)
  :config
  (load-theme 'modus-operandi t))
(use-package multiple-cursors
  :ensure
  :bind (:map mode-specific-map
              ("e" . mc/edit-lines)
              ("A" . mc/mark-all-in-region)))
(use-package orderless
  :ensure
  :custom
  (completion-styles '(substring orderless basic))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism))
  (orderless-style-dispatchers '(negate-if-bang))
  :config
  (eval-after-load "counsel"
    '(setq ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                                   (t . orderless-ivy-re-builder))))
  (defun negate-if-bang (pattern _index _total)
    (when (string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))
(use-package org
  :bind (:map mode-specific-map
              ("l" . org-store-link)
              ("a" . org-agenda)
              ("c" . org-capture)
              ("C" . org-goto-calendar)
              :map org-mode-map
              ("C-TAB" . nil)
              ("C-c ;" . nil))
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :custom
  (org-agenda-span 'fortnight)
  (org-confirm-babel-evaluate nil)
  (org-cycle-separator-lines 0)
  (org-descriptive-links nil)
  (org-edit-src-content-indentation 0)
  (org-edit-src-persistent-message nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-modules nil)
  (org-odd-levels-only nil)
  (org-return-follows-link t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'current-window)
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-use-speed-commands t)
  :config
  ;; #+NAME: embed
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
  ;;
  ;; #+begin_src compile :noweb yes
  ;; <<embed("name", arg1="...", arg2="...", ...)>>
  ;; #+end_src
  (require 'org-tempo nil t)
  (use-package ob-async :ensure)
  (use-package ob-compile
    :bind (:map mode-specific-map ("8" . ob-compile)))
  (defun lazy-load-org-babel-languages (o &rest args)
    (when-let (lang (org-element-property :language (org-element-at-point)))
      (when (or (string= lang "bash") (string= lang "sh")) (setq lang "shell"))
      (unless (cdr (assoc (intern lang) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern lang) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)))
    (apply o args))
  (advice-add 'org-babel-execute-src-block :around #'lazy-load-org-babel-languages))
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
  (put 'savehist-minibuffer-history-variables 'history-length 300)
  (put 'org-read-date-history                 'history-length 300)
  (put 'read-expression-history               'history-length 300)
  (put 'org-table-formula-history             'history-length 300)
  (put 'extended-command-history              'history-length 300)
  (put 'minibuffer-history                    'history-length 300)
  (put 'buffer-name-history                   'history-length 300)
  (put 'file-name-history                     'history-length 300))
(use-package saveplace
  :hook (after-init . save-place-mode)
  :custom
  (save-place-forget-unreadable-files nil))
(use-package shell
  :bind (:map shell-mode-map
              ([remap read-only-mode] . shell-toggle-compile-mode))
  :config
  (defun shell-toggle-compile-mode ()
    (interactive)
    (setq buffer-read-only t)
    (compilation-mode))
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
(use-package string-inflection
  :ensure
  :after embark
  :bind ( :map embark-identifier-map
          ("-" . string-inflection-cycle))
  :config
  (add-to-list 'embark-repeat-actions #'string-inflection-cycle))
(use-package symbol-overlay
  :ensure
  :after embark
  :bind ( :map embark-identifier-map
          ("y" . symbol-overlay-put)))
(use-package tramp
  :defer t
  :custom
  (tramp-auto-save-directory "~/.cache/emacs/backups")
  (tramp-persistency-file-name "~/.emacs.d/data/tramp")
  (tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root")))
  :config
  (put 'temporary-file-directory 'standard-value '("/tmp")))
(use-package tramp-sh
  :custom
  (tramp-ssh-controlmaster-options
   (concat
    "-o ControlPath=~/.ssh/sockets/%%u@%%h:%%p "
    "-o ControlMaster=auto -o ControlPersist=yes"))
  :config
  (add-to-list 'tramp-remote-path "~/bin"))
(use-package tree-sitter
  :ensure
  :diminish
  :hook ((after-init . global-tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode))
  :config
  (use-package tree-sitter-langs :ensure))
(use-package uniquify
  :defer t
  :custom
  (uniquify-after-kill-buffer-p t)
  (uniquify-buffer-name-style 'reverse)
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-separator "|"))
(use-package vertico
  :ensure t
  :bind ( :map vertico-map
          ("?" . minibuffer-completion-help)
          ("C-j" . vertico-exit-input)
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word)
          ("M-G" . vertico-grid-mode)
          ("M-F" . vertico-flat-mode)
          ("M-'" . vertico-quick-insert)
          ("M-m" . vertico-quick-exit)
          ("M-j" . consult-dir)
          ("M-/" . consult-dir-jump-file)
          :map mode-specific-map
          ("C-r" . vertico-repeat))
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :config
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (use-package consult-dir :ensure t))
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
  (wgrep-enable-key "\C-x\C-q")
  :bind ( :map wgrep-mode-map
          ("C-c C-c" . save-buffer)
          :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)))
(use-package wrap-region
  :ensure
  :diminish
  :hook ((after-init . wrap-region-global-mode)
         (wrap-region-after-wrap . move-to-wrapped-region))
  :config
  (defun move-to-wrapped-region ()
    (when (< (point) (region-end))
      (forward-char 1))))
(use-package which-key
  :ensure t
  :functions
  which-key--show-keymap
  which-key--hide-popup-ignore-command)
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
                                    xref-find-references))
  (xref-search-program 'ripgrep))
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

(bind-keys
 ([C-tab]              . other-window)
 ([C-up]               . windmove-up)
 ([C-down]             . windmove-down)
 ([C-left]             . windmove-left)
 ([C-right]            . windmove-right)
 ([remap suspend-frame]. ignore)
 ([remap kill-buffer]  . kill-this-buffer)
 ("C-TAB"              . other-window)
 ("C-`"                . shell)
 ("C-."                . next-error)
 ("C-,"                . previous-error)
 ("M-K"                . kill-this-buffer)
 ("M-o"                . other-window)
 ("M-q"                . fill-paragraph)
 ("RET"                . newline-and-indent)
 ("M-C"                . compile)
 ("M-D"                . recompile)
 ("M-n"                . forward-paragraph)
 ("M-p"                . backward-paragraph)
 ("M-P"                . ff-find-other-file)

 :map help-map
 ("+"                  . package-install)
 ("-"                  . package-delete)
 ("D"                  . toggle-debug-on-error)
 ("p"                  . package-list-packages-no-fetch)
 ("q"                  . delete-other-window)
 ("C-b"                . describe-personal-keybindings)
 ("C-o"                . proced)
 ("="                  . quick-calc)

 :map ctl-x-map
 ("O"                  . ff-find-other-file)
 ("x b"                . bury-buffer)
 ("x e"                . erase-buffer)

 :map mode-specific-map
 ("0"                  . recursive-edit)
 ("SPC"                . cycle-spacing)
 ("r"                  . replace-regexp)
 ("s"                  . replace-string))
