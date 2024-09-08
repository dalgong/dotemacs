;; -*- lexical-binding: t -*-
(setq custom-file "/dev/null")
(advice-add 'custom-save-all :override 'ignore)
(add-to-list 'load-path "~/.emacs.d/lisp")

(dolist (x '( auto-revert-mode delete-selection-mode electric-pair-mode
              global-reveal-mode minibuffer-depth-indicate-mode repeat-mode
              recentf-mode savehist-mode save-place-mode))
  (add-to-list 'after-init-hook x))

(when (cl-loop for p in '(package bind-key use-package) always (require p nil t))
  (nconc package-archives '(("melpa"  . "http://melpa.org/packages/")
                            ("org"    . "http://orgmode.org/elpa/"))))

(use-package emacs
  :bind (([C-tab]              . other-window)
         ([remap suspend-frame]. ignore)
         ([remap kill-buffer]  . kill-this-buffer)
         ([remap list-buffers] . ibuffer)
         ([remap delete-horizontal-space] . cycle-spacing)
         ("RET"                . newline-and-indent)
         ("M-I"                . ff-find-other-file)
         ("M-K"                . kill-this-buffer)
         ("C-c c"              . calendar)
         ("C-h C-o"            . proced))
  :custom
  (async-shell-command-buffer 'rename-buffer)
  (async-shell-command-display-buffer nil)
  (auto-save-default nil)
  (auto-save-interval 0)
  (backup-by-copying t)
  (backup-by-copying-when-linked t)
  (bidi-display-reordering 'left-to-right)
  (bidi-inhibit-bpa t)
  (bidi-paragraph-direction 'left-to-right)
  (column-number-indicator-zero-based nil)
  (completion-auto-help 'visible)
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-auto-select 'second-tab)
  (confirm-nonexistent-file-or-buffer nil)
  (create-lockfiles nil)
  (cycle-spacing-actions '(delete-all-space just-one-space restore))
  (dabbrev-case-fold-search t)
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-no-confirm t)
  (dired-switches-in-mode-line 'as-is)
  (disabled-command-function nil)
  (display-buffer-alist '(("\\*shell\\*" display-buffer-same-window)
                          ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                           display-buffer-at-bottom
                           (window-parameters (mode-line-format . none)))
                          ("\\*hermes.*" display-buffer-same-window)))
  (enable-recursive-minibuffers t)
  (even-window-sizes nil)
  (evil-default-state 'emacs)
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
  (help-char ?^)
  (help-window-select t)
  (history-delete-duplicates t)
  (history-length 1000)
  (ibuffer-expert t)
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (inhibit-startup-echo-area-message (user-login-name))
  (isearch-yank-on-move 'shift)
  (isearch-lazy-count t)
  (isearch-repeat-on-direction-change t)
  (kill-ring-max 3000)
  (kill-whole-line t)
  (lazy-highlight-buffer t)
  (line-move-visual nil)
  (mac-option-key-is-meta t)
  (mac-right-option-modifier nil)
  (make-backup-files nil)
  (mark-even-if-inactive t)
  (mode-line-client nil)
  (mode-line-modified '("%* "))
  (mode-line-remote nil)
  (mode-line-frame-identification nil)
  (mode-line-front-space '("  "))
  (mode-line-position)
  (mode-line-mule-info nil)
  (mode-line-end-spaces nil)
  (ns-alternate-modifier 'super)
  (ns-command-modifier 'meta)
  (ns-tool-bar-display-mode 'both)
  (ns-tool-bar-size-mode 'regular)
  (ns-use-thin-smoothing t)
  (proced-enable-color-flag t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-process-output-max (* 1024 1024))
  (recentf-auto-cleanup (* 3 3600))
  (recentf-max-saved-items 1000)
  (redisplay-skip-fontification-on-input t)
  (remote-file-name-inhibit-locks t)
  (require-final-newline t)
  (revert-without-query '(""))
  (ring-bell-function 'ignore)
  (save-interprogram-paste-before-kill t)
  (scroll-preserve-screen-position t)
  (scroll-margin 0)
  (scroll-step 1)
  (select-active-regions nil)
  (sentence-end-double-space nil)
  (set-mark-command-repeat-pop t)
  (shell-command-switch "-lc")
  (shell-command-default-error-buffer "*Shell Command Errors*")
  (split-height-threshold nil)
  (tab-always-indent 'complete)
  (tab-bar-show nil)
  (tramp-auto-save-directory "~/.cache/emacs/backups")
  (tramp-persistency-file-name "~/.emacs.d/data/tramp")
  (tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root")))
  (use-dialog-box nil)
  (use-package-compute-statistics nil)
  (use-package-always-ensure t)
  (use-short-answers t)
  (vc-follow-symlinks nil)
  (view-read-only t)
  (windmove-wrap-around t)
  (xref-prompt-for-identifier '(not xref-find-definitions
                                    xref-find-definitions-other-window
                                    xref-find-definitions-other-frame
                                    xref-find-references))
  (xref-search-program 'ripgrep)
  (x-underline-at-descent-line t)
  (x-selection-timeout 100)
  (words-include-escapes t)
  :config
  (defun goto-line-with-number ()
    (interactive)
    (setq unread-command-events (cons last-command-event unread-command-events))
    (call-interactively 'goto-line))
  (dotimes (i 10)
    (define-key goto-map (format "%d" i) 'goto-line-with-number))
  (windmove-default-keybindings 'control)
  (set-display-table-slot (or standard-display-table (setq standard-display-table (make-display-table)))
                          'vertical-border (make-glyph-code ?┃))
  (fset 'kill-this-buffer (lambda () (interactive) (kill-buffer (current-buffer))))
  (defvar set-mark-dwim-timeout 0.5)
  (defvar set-mark-dwim-repeat-action 'embark-act)
  (defvar set-mark-dwim-timeout-action 'completion-at-point)
  (defvar-keymap set-mark-dwim-map
    :doc "An briefly active keymap after set-mark-command"
    "SPC" 'embark-select
    "."   'embark-act-all
    "c"   'compile
    "r"   'recompile)
  (defun set-mark-dwim (o &rest args)
    (cond ((or (not (called-interactively-p 'interactive))
               current-prefix-arg
               (memq last-command '(pop-to-mark-command pop-global-mark)))
           (apply o args))
          ((not (sit-for set-mark-dwim-timeout))
           (let ((keyseq (read-key-sequence ""))
                 cmd)
             (cond ((and (setq cmd (lookup-key set-mark-dwim-map keyseq))
                         (commandp cmd))
                    (call-interactively cmd))
                   ((and (setq cmd (lookup-key (current-active-maps) keyseq))
                         (memq cmd '(set-mark-command cua-set-mark)))
                    (call-interactively set-mark-dwim-repeat-action))
                   (t
                    (apply o args)
                    (call-interactively cmd)))))
          (t
           (call-interactively set-mark-dwim-timeout-action))))
  (advice-add 'set-mark-command :around 'set-mark-dwim)
  (defun preseve-window-configuration-if-interactive (o)
    (let ((wc (and (called-interactively-p 'interactive)
                   (current-window-configuration))))
      (unwind-protect (funcall o)
        (and wc (set-window-configuration wc)))))
  (advice-add 'recursive-edit :around 'preseve-window-configuration-if-interactive)
  (defun delete-other-windows-dwim (o &rest args)
    (if (null current-prefix-arg)
        (apply o args)
      (run-at-time 0 nil (lambda () (apply o args)))
      (call-interactively 'recursive-edit)))
  (advice-add 'delete-other-windows :around 'delete-other-windows-dwim)
  (defun call-other-window-if-interactive (&rest _)
    (when (called-interactively-p 'any)
      (other-window 1)))
  (advice-add 'split-window-right :after 'call-other-window-if-interactive)
  (advice-add 'split-window-below :after 'call-other-window-if-interactive)
  (defun switch-to-last-buffer-if-one-window (o &rest args)
    (if (and (one-window-p 'nomini) (called-interactively-p 'interactive))
        (if (= 1 (length (frame-list)))
            (switch-to-buffer nil)
          (other-frame 1))
      (apply o args)))
  (advice-add 'other-window :around 'switch-to-last-buffer-if-one-window)
  (advice-add 'electric-pair-open-newline-between-pairs-psif
              :after (lambda ()
                       (when (eq last-command-event ?\n)
                         (indent-according-to-mode))))
  (defun find-file--line-number (o filename &optional wildcards)
    "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
    (let (line-number)
      (unless (file-exists-p filename)
        (save-match-data
          (when (and (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename)
                     (match-string 2 filename))
            (setq line-number (string-to-number (match-string 2 filename)))
            (setq filename (match-string 1 filename)))))
      (prog1
          (apply o (list filename wildcards))
        (when line-number
          (goto-char (point-min))
          (forward-line (1- line-number))))))
  (advice-add 'find-file :around 'find-file--line-number)
  (use-package ffap
    :ensure nil
    :config
    (defun ffap-file-at-point-add-line-number (r)
      (let ((s (ffap-string-at-point)))
        (save-match-data
          (if (string-match "\\(:[0-9]+\\)\\(:[0-9]+\\)?$" s)
              (concat r (match-string 1 s))
            r))))
    (advice-add 'ffap-file-at-point :filter-return 'ffap-file-at-point-add-line-number)))
(use-package delight :config (delight '((auto-revert-mode "" autorevert) (eldoc-mode "" eldoc))))
(use-package avy
  :bind (("M-o" . avy-goto-char-timer) :map isearch-mode-map ("M-o" . avy-isearch))
  :config
  (avy-setup-default)
  (defun avy-pop-mark-if-prefix (o &rest args)
    (if current-prefix-arg
        (call-interactively 'avy-pop-mark)
      (apply o args)))
  (advice-add 'avy-goto-char-timer :around 'avy-pop-mark-if-prefix))
(use-package beardbolt
  :vc ( :url "https://github.com/joaotavora/beardbolt.git"
        :rev :newest)
  :bind ("C-c :" . beardbolt-starter)
  :config
  (push (cons 'c++-ts-mode (cdr (assq 'c++-mode beardbolt-languages))) beardbolt-languages))
(use-package cape
  :config
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))
(use-package compile
  :bind (("M-C" . compile) ("M-R" . recompile))
  :custom
  (compilation-environment '("TERM=xterm-256color"))
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-buffer-name-function 'get-idle-compilation--buffer-name)
  (compilation-save-buffers-predicate 'ignore)
  :config
  (advice-add 'compilation-start :around 'maybe-eat-compilation-start)
  (defun do-kill-compilation (o &rest args)
    (when (and (called-interactively-p 'interactive)
               (memq major-mode '(comint-mode compilation-mode eat-mode))
               (get-buffer-process (current-buffer)))
      (kill-compilation)
      (while (get-buffer-process (current-buffer))
        (sit-for .5)))
    (apply o args))
  (advice-add 'recompile :around 'do-kill-compilation)
  (defun get-idle-compilation--buffer-name (name-of-mode)
    (let ((name (compilation--default-buffer-name name-of-mode)))
      (or (cl-loop for b in (buffer-list)
                   with name-re = (concat "^" (regexp-quote name))
                   when (and (string-match name-re (buffer-name b))
                             (not (process-live-p (get-buffer-process b))))
                   return (buffer-name b))
          (generate-new-buffer-name name)))))
(use-package consult
  :bind (("M-\"" . consult-register-load)
         ("M-'"  . consult-register-store)
         ([remap yank-pop] . consult-yank-pop)
         ("M-T" . consult-imenu)
         ("C-c k"   . consult-kmacro)
         ("C-x M-:" . consult-complex-command)
         ("C-x b"   . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-x m"   . consult-mode-command)
         ("C-x r b" . consult-bookmark)
         ("C-h C-i"   . consult-info)
         ("C-h C-m"   . consult-man)
         ("C-h SPC"   . consult-mark)
         ("C-h C-SPC" . consult-global-mark)

         :map minibuffer-local-map
         ("M-r"   . consult-history)

         :map goto-map
         ("d"   . consult-imenu)
         ("e"   . consult-compile-error)
         ("f"   . consult-flymake)

         :map search-map
         ("e"    . consult-isearch-history)
         ("f"    . consult-find)
         ("g"    . grep)
         ("G"    . consult-git-grep)
         ("m"    . consult-line-multi)
         ("l"    . consult-line)
         ("o"    . consult-outline)
         ("k"    . consult-keep-lines)
         ("r"    . consult-ripgrep)
         ("u"    . consult-focus-lines)

         :map isearch-mode-map
         ("M-h"  . consult-isearch-history)
         ("M-l"  . consult-line)
         ("M-L"  . consult-line-multi)
         ("M-q"  . isearch-query-replace))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (register-preview-delay 0.5)
  (register-preview-function 'consult-register-format)
  (completion-in-region-function 'consult-completion-in-region)
  (consult-narrow-key "<")
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref)
  :config
  (advice-add 'register-preview :override 'consult-register-window)
  (defun consult-imenu-across-all-buffers (o &rest args)
    (if current-prefix-arg
        (call-interactively 'consult-imenu-multi)
      (apply o args)))
  (advice-add 'consult-imenu :around 'consult-imenu-across-all-buffers))
(use-package corfu
  :hook (prog-mode text-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-quit-no-match t)
  :config
  (use-package corfu-terminal
    :config
    (unless (display-graphic-p)
      (corfu-terminal-mode +1))))
(use-package display-line-numbers :hook (prog-mode text-mode))
(use-package easy-kill
  :after embark
  :bind (([remap kill-ring-save] . easy-kill))
  :config
  (defun embark-target-easy-kill-region ()
    "Target the region if active. easy-kill region."
    (let ((r (ignore-errors (easy-kill-get bounds))))
      (when (and r (car r))
        (let ((start (car r))
            (end (cdr r)))
        `(region ,(buffer-substring start end) . ,r)))))
  (add-to-list 'embark-target-finders 'embark-target-easy-kill-region))
(use-package eat
  :vc ( :url "https://codeberg.org/akib/emacs-eat"
        :rev :newest)
  :autoload maybe-eat-compilation-start
  :commands (eat-emacs-mode eat-mode)
  :functions (eat-exec eat-term-send-string-as-yank eat--synchronize-scroll-windows)
  :bind (("C-`" . eat) :map eat-mode-map ("C-z" . eat-toggle-char-mode))
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode))
  :custom
  (eat-shell-prompt-annotation-position 'right-margin)
  :init
  (defun override-eat-term-keymap (map)
    (define-key map (kbd "M-o")  'avy-goto-char-timer)
    (define-key map (kbd "M-\"") 'consult-register-load)
    (define-key map (kbd "C-z")  'eat-toggle-char-mode)
    map)
  (advice-add 'eat-term-make-keymap :filter-return 'override-eat-term-keymap)
  :config
  (defun eat-dwim (o &rest args)
    (if (or (car args)
            (cadr args)
            (not (called-interactively-p 'any))
            (not (eq major-mode 'eat-mode)))
        (apply o args)
      (bury-buffer)))
  (advice-add 'eat :around 'eat-dwim)
  (defun eat-toggle-char-mode ()
    (interactive)
    (call-interactively (if eat--semi-char-mode
                            'eat-emacs-mode
                          'eat-semi-char-mode)))
  (defun eat-insert-for-yank (o &rest args)
    (if (null (ignore-errors eat-terminal))
        (apply o args)
      (funcall eat--synchronize-scroll-function
               (eat--synchronize-scroll-windows 'force-selected))
      (eat-term-send-string-as-yank
       eat-terminal
       (let ((yank-hook (bound-and-true-p yank-transform-functions)))
         (with-temp-buffer
           (setq-local yank-transform-functions yank-hook)
           (apply o args)
           (buffer-string))))))
  (advice-add 'insert-for-yank :around 'eat-insert-for-yank)
  (defun maybe-eat-compilation-start (o &rest args)
    (apply (if (eq (cadr args) 'grep-mode) o 'eat-compilation-start) args))
  (advice-add 'compilation-start :around 'maybe-eat-compilation-start)
  (defun eat-compilation-start (command &optional mode name-function _ _)
    (let ((name-of-mode "compilation")
          (dir default-directory)
          outbuf)
      (if (or (not mode) (eq mode t))
          (setq mode 'compilation-minor-mode)
        (setq name-of-mode (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))
      (with-current-buffer
          (setq outbuf
                (get-buffer-create
                 (compilation-buffer-name name-of-mode mode name-function)))
        (setq default-directory dir)
        (setq buffer-read-only nil)
        (erase-buffer)
        (compilation-insert-annotation
         "-*- mode: " name-of-mode
         "; default-directory: "
         (prin1-to-string (abbreviate-file-name default-directory))
         " -*-\n")
        (compilation-insert-annotation
         (format "%s started at %s\n\n"
                 mode-name
                 (substring (current-time-string) 0 19))
         command "\n")
        (eat-mode)
        (eat-exec outbuf "*compile*" shell-file-name nil (list "-lc" command))
        (run-hook-with-args 'compilation-start-hook (get-buffer-process outbuf))
        (eat-emacs-mode)
        (set (make-local-variable 'eat--synchronize-scroll-function)
             'eat--eshell-synchronize-scroll)
        (funcall mode)
        (setq-local compilation-directory dir)
        (setq-local compile-command command)
        (setq-local compilation-arguments (list command (if (eq mode 'compilation-minor-mode) nil mode) name-function))
        (setq-local revert-buffer-function 'compilation-revert-buffer)
        (setq next-error-last-buffer outbuf)
        (display-buffer outbuf '(nil (allow-no-window . t)))))))
(use-package ediff
  :bind (:map goto-map ("=" . ediff-current-file))
  :custom
  (ediff-custom-diff-options "-u")
  (ediff-keep-variants nil)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (defun silently (o &rest args)
    (cl-letf (((symbol-function 'y-or-n-p) #'(lambda (_) t)))
      (apply o args)))
  (advice-add 'ediff-janitor :around 'silently)
  (advice-add 'ediff-quit :around 'silently))
(use-package embark
  :commands (embark-act embark-prefix-help-command)
  :functions embark--targets
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         :map minibuffer-local-map
         ("M-E" . embark-export)
         ("M-S" . embark-collect)
         :map embark-region-map
         ("!" . shell-command)
         ("x" . compile)
         :map help-map
         ("b" . embark-bindings))
  :custom
  (embark-confirm-act-all nil)
  (embark-cycle-key "C-SPC")
  (prefix-help-command 'embark-prefix-help-command)
  (embark-help-key "?")
  (embark-quit-after-action nil)
  :config
  (setq embark-indicators (delq 'embark-mixed-indicator embark-indicators))
  (add-to-list 'embark-indicators 'embark-minimal-indicator)
  (add-to-list 'embark-post-action-hooks '(kill-this-buffer embark--restart))
  (push 'embark--xref-push-marker (alist-get 'find-file embark-pre-action-hooks)))
(use-package embark-consult :after consult)
(static-if (memq window-system '(mac ns))
    (use-package exec-path-from-shell
  :hook (after-init . exec-path-from-shell-initialize)))
(use-package go-mode
  :functions (gofmt)
  :custom (gofmt-command "goimports")
  :hook (go-mode . eglot-ensure))
(use-package go-ts-mode
  :hook ((go-ts-mode . eglot-ensure)
         (before-save . gofmt-before-save))
  :config
  (advice-add 'gofmt-before-save :around
              (defun gofmt-on-go-ts-mode (o &rest args)
                (if (eq major-mode 'go-ts-mode)
                    (gofmt)
                  (apply o args)))))
(use-package hl-line :hook (prog-mode conf-mode compilation-mode eat-mode text-mode))
(use-package iedit :bind (("C-c E" . iedit-mode) :map isearch-mode-map ("M-e" . iedit-mode-from-isearch)))
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism)))
(use-package org
  :bind (:map org-mode-map ("C-TAB" . nil) ("C-c ;" . nil))
  :hook (org-babel-after-execute . org-redisplay-inline-images)
  :custom
  (org-confirm-babel-evaluate nil)
  (org-cycle-separator-lines 0)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-modules nil)
  (org-return-follows-link t)
  (org-src-window-setup 'current-window)
  (org-startup-indented t)
  (org-use-speed-commands t)
  :config
  (require 'org-tempo nil t)
  (use-package ob-async)
  (use-package ob-compile :ensure nil)
  (defun lazy-load-org-babel-languages (o &rest args)
    (when-let (lang (org-element-property :language (org-element-at-point)))
      (when (or (string= lang "bash") (string= lang "sh")) (setq lang "shell"))
      (unless (cdr (assoc (intern lang) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern lang) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)))
    (apply o args))
  (advice-add 'org-babel-execute-src-block :around 'lazy-load-org-babel-languages)
  (defun fix-missing-args (o &rest args)
    (when (> (length args) 4)
      (setf (nthcdr 4 args) nil))
    (apply o args))
  (advice-add #'ob-async-org-babel-execute-src-block :around 'fix-missing-args))
(use-package pdf-tools
  :if window-system
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))
(use-package python
  :ensure nil
  :hook (python-ts-mode . maybe-set-python-shell-virtualenv-root)
  :custom (python-shell-interpreter "ipython")
  :config
  (defun maybe-set-python-shell-virtualenv-root ()
    (when-let (dir (and (null python-shell-virtualenv-root)
                        (locate-dominating-file default-directory ".venv/")))
      (setq-local python-shell-virtualenv-root (expand-file-name ".venv/" dir)))))
(use-package smerge-mode
  :after embark
  :hook (find-file . smerge-start-session)
  :config
  (defvar embark-vc-conflict-map (make-composed-keymap smerge-basic-map embark-general-map))
  (defun embark-vc-target-conflict-at-point ()
    "Target a Merge Conflict at point."
    (when-let* ((smerge-mode smerge-mode)
                (b (save-excursion
                     (end-of-line)
                     (and (re-search-backward smerge-begin-re nil t) (pos-bol))))
                (e (save-excursion
                     (and (re-search-forward smerge-end-re nil t) (pos-eol 0))))
                (in-range (<= b (point) e)))
      `(conflict "hunk" ,b . ,e)))
  (add-to-list 'embark-target-finders 'embark-vc-target-conflict-at-point)
  (add-to-list 'embark-keymap-alist '(conflict . embark-vc-conflict-map)))
(use-package tab-bar-echo-area :hook after-init)
(static-if (and (fboundp 'treesit-available-p) (treesit-available-p))
    (use-package treesit-auto
      :hook ((after-init . global-treesit-auto-mode)
             (prog-mode . fix-forward-sexp-function))
      :custom
      (treesit-font-lock-level 4)
      :config
      (defun fix-forward-sexp-function () (setq forward-sexp-function nil))))
(use-package vertico
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :bind (("C-c C-r" . vertico-repeat)
         :map vertico-map
         ("C-j"     . vertico-exit-input)
         ("DEL"     . vertico-directory-delete-char)
         ("M-/"     . consult-find-dwim)
         ("C-`"     . command-here)
         ("M-s g"   . command-here)
         ("M-s r"   . command-here))
  :custom
  (vertico-count-format nil)
  :config
  (defun vertico-selected-directory ()
    (vertico-insert)
    (file-name-directory (substitute-in-file-name (minibuffer-contents-no-properties))))
  (defun command-here ()
    (interactive)
    (let ((dir (vertico-selected-directory))
          (cmd (lookup-key global-map (this-command-keys))))
      (run-at-time 0 nil (lambda () (let ((default-directory dir))
                                      (call-interactively cmd))))
      (abort-recursive-edit)))
  (defun consult-find-dwim ()
    (interactive)
    (run-at-time 0 nil #'consult-find (vertico-selected-directory))
    (abort-recursive-edit))
  (use-package vertico-multiform
    :ensure nil
    :config
    (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
    (vertico-multiform-mode 1)))
(use-package vundo :bind ("C-x u" . vundo))
(use-package wgrep :custom (wgrep-auto-save-buffer t))
