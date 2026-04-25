;; -*- lexical-binding: t -*-
(setq custom-file null-device)
(advice-add 'custom-save-all :override 'ignore)
(add-to-list 'load-path "~/.emacs.d/lisp")

(mapc #'require '(package bind-key use-package))
(push '("melpa" . "http://melpa.org/packages/") package-archives)

(use-package emacs
  :ensure nil
  :bind (([remap list-buffers]   . ibuffer)
         ([rempa kill-buffer]    . kill-current-buffer)
         ([remap dabbrev-expand] . hippie-expand)
         ("RET"                  . newline-and-indent)
         ("M-K"                  . kill-current-buffer)
         ("M-o"                  . other-window)
         ("M-0"                  . delete-window)
         ("M-1"                  . delete-other-windows)
         ("M-2"                  . split-window-below)
         ("M-3"                  . split-window-right)
         ("M-9"                  . quit-window)
         ("C-x \\"               . align-regexp)
         ("C-c c"                . calendar)
         ("C-c w"                . world-clock)
         ("C-c r"                . query-replace)
         ("C-c C-r"              . query-replace-regexp)
         ("C-h C-o"              . proced))
  :custom
  (async-shell-command-buffer 'rename-buffer)
  (auto-save-interval 0)
  (bidi-display-reordering 'left-to-right)
  (bidi-inhibit-bpa t)
  (bidi-paragraph-direction 'left-to-right)
  (completion-auto-select)
  (completion-category-overrides '((file (styles partial-completion))))
  (completions-format 'vertical)
  (completions-sort 'historical)
  (create-lockfiles nil)
  (cursor-in-non-selected-windows nil)
  (disabled-command-function nil)
  (display-buffer-alist '(("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                           display-buffer-at-bottom
                           (window-parameters (mode-line-format . none)))
                          ("\\*hermes.*" display-buffer-same-window)))
  (electric-pair-mode t)
  (enable-recursive-minibuffers t)
  (ffap-machine-p-known 'reject)
  (help-window-select t)
  (hippie-expand-try-functions-list '(try-expand-dabbrev
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev-from-kill
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-expand-all-abbrevs
                                      try-expand-list
                                      try-expand-line
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol))
  (ibuffer-expert t)
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (inhibit-startup-echo-area-message (user-login-name))
  (isearch-yank-on-move 'shift)
  (isearch-lazy-count t)
  (kill-do-not-save-duplicates t)
  (kill-whole-line t)
  (large-file-warning-threshold 100000000)
  (mac-option-key-is-meta t)
  (make-backup-files nil)
  (max-mini-window-height 0.2)
  (minibuffer-depth-indicate-mode t)
  (mode-line-end-spaces nil)
  (mode-line-frame-identification nil)
  (mode-line-position '((-3 "%p") " %l:%c"))
  (mode-line-modified '("%* "))
  (mode-line-mule-info nil)
  (mode-line-remote nil)
  (ns-command-modifier 'meta)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-process-output-max (* 4 1024 1024))
  (recentf-mode t)
  (recentf-max-saved-items 1000)
  (redisplay-skip-fontification-on-input t)
  (repeat-mode t)
  (require-final-newline t)
  (revert-without-query '(""))
  (ring-bell-function 'ignore)
  (savehist-additional-variables '(search-ring regexp-search-ring kill-ring))
  (save-interprogram-paste-before-kill t)
  (scroll-conservatively 5)
  (select-active-regions nil)
  (sentence-end-double-space nil)
  (set-mark-command-repeat-pop t)
  (shell-command-switch "-lc")
  (split-height-threshold nil)
  (tab-always-indent 'complete)
  (truncate-lines t)
  (use-dialog-box nil)
  (use-package-compute-statistics nil)
  (use-package-always-ensure t)
  (use-short-answers t)
  (vc-follow-symlinks nil)
  (view-read-only t)
  (window-combination-resize t)
  (xref-search-program 'ripgrep)
  :config
  (dolist (m '(delete-selection-mode global-visual-wrap-prefix-mode minibuffer-regexp-mode
               save-place-mode winner-mode savehist-mode))
    (add-hook 'after-init-hook m))
  (defun strip-string-properties ()
    (setq kill-ring (mapcar #'substring-no-properties (cl-remove-if-not #'stringp kill-ring))))
  (add-hook 'savehist-save-hook 'strip-string-properties)
  (windmove-default-keybindings 'control)
  (or standard-display-table (setq standard-display-table (make-display-table)))
  (set-display-table-slot standard-display-table 'vertical-border ?\u2502)
  (set-display-table-slot standard-display-table 'truncation ?\u2192)
  (defun toggle-delete-other-windows ()
    (interactive)
    (if (and winner-mode (equal (selected-window) (next-window)))
      (with-no-warnings (winner-undo))
    (delete-other-windows)))
  (define-key global-map [remap delete-other-windows] 'toggle-delete-other-windows)
  (defvar set-mark-dwim-timeout 0.5)
  (defvar set-mark-dwim-repeat-action 'embark-act)
  (defvar set-mark-dwim-timeout-action 'completion-at-point)
  (defvar-keymap set-mark-dwim-map
    :doc "An briefly active keymap after set-mark-command"
    :repeat t
    "SPC" #'hippie-expand)
  (advice-add 'set-mark-command :around 'set-mark-dwim)
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
                    (call-interactively (setq this-command cmd)))
                   ((and (setq cmd (lookup-key (current-active-maps) keyseq))
                         (memq cmd '(set-mark-command cua-set-mark)))
                    (call-interactively set-mark-dwim-repeat-action))
                   (t
                    (apply o args)
                    (call-interactively cmd)))))
          (t
           (call-interactively set-mark-dwim-timeout-action))))
  (defvar M-O-cmd 'ff-find-other-file)
  (defun M-O-dwim ()
    (interactive)
    (if (sit-for 0.1)
        (call-interactively (setq this-command M-O-cmd))
      (with-no-warnings
        (set-transient-map xterm-function-map))
      (setq unread-command-events (append '(?\e ?O) unread-command-events))))
  (define-key global-map (kbd "M-O") 'M-O-dwim)
  (defun call-other-window-if-interactive (&rest _)
    (when (called-interactively-p 'any)
      (other-window 1)))
  (advice-add 'split-window-right :after 'call-other-window-if-interactive)
  (advice-add 'split-window-below :after 'call-other-window-if-interactive)
  (advice-add 'other-window :before
              (lambda (&rest _)
                (when (and (one-window-p 'nomini) (called-interactively-p 'interactive))
                  (if (or (not window-system) (= 1 (length (frame-list))))
                      (switch-to-buffer nil)
                    (other-frame 1)))))
  (advice-add 'electric-pair-open-newline-between-pairs-psif :after
              (lambda ()
                (when (eq last-command-event ?\n)
                  (indent-according-to-mode)))))
(use-package delight
  :config
  (delight '((auto-revert-mode "" autorevert)
             (eldoc-mode "" eldoc)
             (outline-minor-mode "" outline)
             (buffer-face-mode "" face-remap))))
(use-package avy
  :bind (("C-'" . avy-goto-char-timer) :map isearch-mode-map ("C-'" . avy-isearch))
  :custom
  (avy-background t)
  :config
  (advice-add 'avy-goto-char-timer :before-until
              (lambda (&optional _)
                (when current-prefix-arg
                  (call-interactively 'avy-pop-mark)))))
(use-package browse-url
  :defer t
  :config
  (advice-add 'browse-url-default-browser :before-until
              (lambda (url &rest _)
                (when (getenv "BROWSER")
                  (call-process (getenv "BROWSER") nil nil nil url)
                  t))))
(use-package cape
  :bind
  (:map set-mark-dwim-map ("/" . cape-dabbrev))
  :config
  (setq completion-at-point-functions
        (nconc completion-at-point-functions
               '(cape-history cape-file cape-keyword cape-dabbrev cape-elisp-block))))
(use-package clipetty
  :delight
  :hook (after-init . global-clipetty-mode))
(use-package compile
  :bind (("M-C" . compile) ("M-R" . recompile))
  :custom
  (compilation-environment '("TERM=xterm-256color"))
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-buffer-name-function 'get-idle-compilation--buffer-name)
  (compilation-save-buffers-predicate 'ignore)
  :config
  (defun get-idle-compilation--buffer-name (name-of-mode)
    (let ((name (compilation--default-buffer-name name-of-mode)))
      (or (cl-loop for b in (buffer-list)
                   with name-re = (concat "^" (regexp-quote name))
                   when (and (string-match name-re (buffer-name b))
                             (not (process-live-p (get-buffer-process b))))
                   return (buffer-name b))
          (generate-new-buffer-name name)))))
(use-package consult
  :bind (("C-;" . consult-register-load)
         ("C-:" . consult-register-store)
         ("M-T" . consult-imenu)
         ([remap yank-pop] . consult-yank-pop)
         ("C-x b"   . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-h C-i"   . consult-info)
         ("C-h SPC"   . consult-mark)
         ("C-h C-SPC" . consult-global-mark)
         :map minibuffer-local-map
         ("M-r"   . consult-history)
         :map goto-map
         ("f" . consult-flymake)
         ("o" . consult-outline)
         :map search-map
         ("e"    . consult-isearch-history)
         ("g"    . grep)
         ("G"    . consult-git-grep)
         ("r"    . consult-ripgrep)
         :map isearch-mode-map
         ("M-o"  . consult-line)
         ("M-l"  . consult-line-multi)
         ("M-h"  . consult-isearch-history)
         ("M-q"  . isearch-query-replace))
  :custom
  (register-preview-delay 0.5)
  (register-preview-function 'consult-register-format)
  (completion-in-region-function 'consult-completion-in-region)
  (consult-narrow-key "<")
  (xref-show-xrefs-function 'consult-xref)
  (xref-show-definitions-function 'consult-xref)
  :config
  (advice-add 'register-preview :override 'consult-register-window)
  (advice-add 'consult-imenu :before-until
              (lambda ()
                (when current-prefix-arg
                  (call-interactively 'consult-imenu-multi)
                  t))))
(use-package completion-preview
  :delight
  :ensure nil
  :hook (prog-mode text-mode)
  :bind ( :map completion-preview-active-mode-map
          ("M-n" . completion-preview-next-candidate)
          ("M-p" . completion-preview-prev-candidate)))
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers--turn-on))
(use-package diffview :after diff-mode :bind (:map diff-mode-map ("|" . diffview-current)))
(use-package easy-kill
  :after embark
  :bind ([remap kill-ring-save] . easy-kill)
  :config
  (defun embark-target-easy-kill-region ()
    "Target the region if active. easy-kill region."
    (let ((r (ignore-errors (with-no-warnings (easy-kill-get bounds)))))
      (when (and r (car r))
        (let ((start (car r))
            (end (cdr r)))
        `(region ,(buffer-substring start end) . ,r)))))
  (with-no-warnings
    (add-to-list 'embark-target-finders 'embark-target-easy-kill-region)))
(use-package ediff
  :bind (:map goto-map ("=" . ediff-current-file))
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain))
(use-package eglot
  :defer t
  :config
  (add-to-list 'eglot-stay-out-of 'imenu))
(use-package embark
  :commands (embark-act embark-prefix-help-command)
  :functions embark--targets
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         :map minibuffer-local-map
         ("M-E" . embark-export)
         :map embark-region-map
         ("x" . compile))
  :custom
  (prefix-help-command 'embark-prefix-help-command)
  :config
  (setq embark-indicators (delq 'embark-mixed-indicator embark-indicators))
  (add-to-list 'embark-indicators 'embark-minimal-indicator)
  (add-to-list 'embark-post-action-hooks '(kill-current-buffer embark--restart))
  (push 'embark--xref-push-marker (alist-get 'find-file embark-pre-action-hooks)))
(use-package exec-path-from-shell
  :hook (after-init . exec-path-from-shell-initialize))
(use-package eyebrowse
  :hook (after-init . eyebrowse-mode)
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-z"))
  :bind ( :map eyebrowse-mode-map
          ("C-M-." . eyebrowse-next-window-config)
          ("C-M-," . eyebrowse-prev-window-config)
          :map eyebrowse-mode-prefix-map
          ("C-z" . eyebrowse-last-window-config))
  :custom
  (eyebrowse-new-workspace t)
  :config
  (defvar eyebrowse-slot-to-register-function 'ignore)
  (cl-loop for i from 0 to 9 do (define-key global-map (read-kbd-macro (format "C-c %d" i)) 'eyebrowse-dwim))
  (defun eyebrowse-dwim ()
    (interactive)
    (let* ((c (logand ?\xff last-command-event))
           (slot (- c ?0))
           (switch-only (eyebrowse--window-config-present-p slot))
           (reg (funcall eyebrowse-slot-to-register-function
                         (if (= slot 0) (eyebrowse--get 'current-slot) slot))))
      (unless (= slot 0)
        (eyebrowse-switch-to-window-config slot))
      (when (and (not switch-only) reg (get-register reg))
        (jump-to-register reg)))))
(use-package ghostel
  :bind (("C-`" . ghostel)
         :map ghostel-mode-map
         ("C-." . ghostel-copy-mode)
         :map ghostel-copy-mode-map
         ("M-w" . nil)
         ("C-." . ghostel-copy-mode-exit))
  :hook (after-init . ghostel-compile-global-mode)
  :config
  (require 'ghostel-fixes nil t)
  (when (require 'ghostel-compile nil t)
    (define-key global-map (kbd "C-c C-k") 'kill-compilation)
    (advice-add 'ghostel-compile--start :filter-args
                (defun move-to-project-root (r)
                  (when (project-current)
                    (setf (caddr r) (project-root (project-current))))
                  r))))
(use-package go-ts-mode
  :hook ((go-ts-mode . eglot-ensure)
         (before-save . gofmt-before-save))
  :config
  (use-package go-mode :functions (gofmt) :custom (gofmt-command "goimports"))
  (defun gofmt-before-save () (when (derived-mode-p 'go-mode) (gofmt))))
(use-package hl-line :hook (prog-mode conf-mode compilation-mode text-mode))
(use-package icomplete
  :hook (after-init . icomplete-vertical-mode)
  :custom
  (icomplete-compute-delay 0)
  (icomplete-hide-common-prefix nil)
  (icomplete-in-buffer t)
  (icomplete-matches-format nil)
  (icomplete-prospects-height 10)
  (icomplete-show-matches-on-no-input t)
  (icomplete-tidy-shadowed-file-names t)
  :bind ( :map icomplete-minibuffer-map
          ("C-."    . embark-act)
	  ("DEL"    . icomplete-fido-backward-updir)
	  ("RET"    . icomplete-force-complete-and-exit)
          ("C-j"    . icomplete-ret)
          ("TAB"    . icomplete-force-complete)
          ("<down>" . icomplete-forward-completions) ("C-n"    . icomplete-forward-completions)
          ("<up>"   . icomplete-backward-completions)("C-p"    . icomplete-backward-completions)
	  ("M-/"    . (lambda () (interactive) (command-here 'consult-find)))
          ("C-`"    . command-here)
          ("C-z"    . command-here)
          ("M-s g"  . command-here)
          ("M-s r"  . command-here))
  :config
  (setq icomplete-scroll t)
  (advice-add 'icomplete-ret :before
              (lambda ()
                (when (equal (icomplete--field-string) icomplete--initial-input)
                  (exit-minibuffer))))
  (advice-add 'completion-at-point :after 'minibuffer-hide-completions)
  (defun command-here (&optional cmd)
    (interactive)
    (icomplete-force-complete)
    (run-at-time
     0 nil
     (lambda (cmd dir) (let ((default-directory dir))
                         (call-interactively cmd)))
     (or cmd (lookup-key global-map (this-command-keys)))
     (file-name-directory (substitute-in-file-name (minibuffer-contents-no-properties))))
    (abort-recursive-edit)))
(use-package iedit :bind (("C-c E" . iedit-mode) :map isearch-mode-map ("M-e" . iedit-mode-from-isearch)))
(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :hook (after-init . marginalia-mode))
(use-package markdown-indent-mode
  :hook (markdown-ts-mode . markdown-indent-mode)
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-spaces-after-code-fence 0)
  :delight)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism)))
(use-package org
  :bind (:map org-mode-map ("C-TAB" . nil) ("C-c ;" . nil) ("C-," . nil))
  :custom
  (org-confirm-babel-evaluate nil)
  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-modules nil)
  (org-return-follows-link t)
  (org-startup-indented t)
  (org-use-speed-commands t)
  :config
  (require 'org-tempo nil t)
  (require 'ob-compile nil)
  (advice-add 'org-babel-execute-src-block :before
              (lambda (&rest _)
                (when-let* ((lang (with-no-warnings
                                    (org-element-property :language (org-element-at-point)))))
                  (when (or (string= lang "bash") (string= lang "sh")) (setq lang "shell"))
                  (unless (cdr (assoc (intern lang) org-babel-load-languages))
                    (add-to-list 'org-babel-load-languages (cons (intern lang) t))
                    (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))))))
(use-package outline-indent
  :delight outline-indent-minor-mode
  :bind (:map outline-indent-minor-mode-map ("S-<tab>" . outline-toggle-children))
  :hook (prog-mode . outline-indent-minor-mode))
(use-package pdf-tools
  :if window-system
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))
(use-package python
  :ensure nil
  :hook
  (python-ts-mode . maybe-set-python-shell-virtualenv-root)
  :custom
  (python-shell-interpreter "uv")
  (python-shell-interpreter-args "run ipython -i")
  (python-shell-prompt-detect-failure-warning nil)
  :config
  (defun maybe-set-python-shell-virtualenv-root ()
    (when-let* ((dir (and (null python-shell-virtualenv-root)
                          (locate-dominating-file default-directory ".venv/"))))
      (setq-local python-shell-virtualenv-root (expand-file-name ".venv/" dir)))))
(use-package treesit-auto
  :hook ((after-init . global-treesit-auto-mode))
  :custom
  (treesit-font-lock-level 4))
(use-package vundo :bind ("C-x u" . vundo))
