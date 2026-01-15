;; -*- lexical-binding: t -*-
(setq custom-file null-device)
(setq native-comp-speed -1)
(advice-add 'custom-save-all :override 'ignore)
(add-to-list 'load-path "~/.emacs.d/lisp")

(mapc #'require '(package bind-key use-package))
(push '("melpa" . "http://melpa.org/packages/") package-archives)

(use-package emacs
  :bind (([remap kill-buffer]  . kill-current-buffer)
         ([remap list-buffers] . ibuffer)
         ([remap delete-horizontal-space] . cycle-spacing)
         ("S-<mouse-1>"        . ffap-at-mouse)
         ("RET"                . newline-and-indent)
         ("M-K"                . kill-current-buffer)
         ("M-o"                . other-window)
         ("C-c c"              . calendar)
         ("C-c r"              . query-replace)
         ("C-c q"              . quit-window)
         ("C-h C-o"            . proced))
  :custom
  (async-shell-command-buffer 'rename-buffer)
  (auto-save-default nil)
  (auto-save-interval 0)
  (bidi-inhibit-bpa t)
  (bidi-paragraph-direction t)
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (confirm-nonexistent-file-or-buffer nil)
  (create-lockfiles nil)
  (cycle-spacing-actions '(delete-all-space just-one-space restore))
  (delete-selection-mode t)
  (dired-no-confirm t)
  (dired-switches-in-mode-line 'as-is)
  (disabled-command-function nil)
  (display-buffer-alist '(("\\*\\(shell\\|eat\\)\\*" display-buffer-same-window)
                          ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                           display-buffer-at-bottom
                           (window-parameters (mode-line-format . none)))
                          ("\\*hermes.*" display-buffer-same-window)))
  (electric-pair-mode t)
  (enable-recursive-minibuffers t)
  (global-auto-revert-mode t)
  (global-reveal-mode t)
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
  (minibuffer-depth-indicate-mode t)
  (mode-line-end-spaces nil)
  (mode-line-frame-identification nil)
  (mode-line-position '((-3 "%p") " %l:%c"))
  (mode-line-modified '("%* "))
  (mode-line-mule-info nil)
  (mode-line-remote nil)
  (ns-alternate-modifier 'super)
  (ns-command-modifier 'meta)
  (proced-enable-color-flag t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-process-output-max (* 1024 1024))
  (recentf-mode t)
  (recentf-auto-cleanup (* 3 3600))
  (recentf-max-saved-items 1000)
  (remote-file-name-inhibit-locks t)
  (repeat-mode t)
  (require-final-newline t)
  (revert-without-query '(""))
  (ring-bell-function 'ignore)
  (scroll-conservatively 5)
  (select-active-regions nil)
  (sentence-end-double-space nil)
  (set-mark-command-repeat-pop t)
  (shell-command-switch "-lc")
  (split-height-threshold nil)
  (tab-always-indent 'complete)
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
  (words-include-escapes t)
  :config
  (setq after-init-hook (nconc after-init-hook '(savehist-mode save-place-mode)))
  (defvar first-key-overload-command-list '(find-file execute-extended-command))
  (defvar-keymap first-key-overload-map :doc "Mimic vscode behavior")
  (defun self-insert-dwim (n &optional c)
    (interactive (list (prefix-numeric-value current-prefix-arg) last-command-event))
    (if-let* ((cmd (and (memq last-command first-key-overload-command-list)
                        (lookup-key first-key-overload-map (vector last-command-event)))))
        (progn (run-at-time 0 nil #'call-interactively cmd)
               (abort-recursive-edit)))
    (self-insert-command n c))
  (eval-and-compile
    (defmacro minibuffer-local-first-keys (&rest bindings)
      `(progn
         ,@(mapcar (lambda (b)
                     `(keymap-set first-key-overload-map ,(car b) ',(cdr b)))
                   bindings)
         ,@(mapcar (lambda (b)
                     `(keymap-set minibuffer-local-map ,(car b) 'self-insert-dwim))
                   bindings))))
  (minibuffer-local-first-keys
   (">" . execute-extended-command)
   ("@" . consult-imenu)
   (":" . consult-goto-line)
   (";" . consult-outline))  
  (windmove-default-keybindings 'control)
  (or standard-display-table (setq standard-display-table (make-display-table)))
  (set-display-table-slot standard-display-table 'vertical-border ?\u2502)
  (set-display-table-slot standard-display-table 'truncation ?\u2192)
  (defvar set-mark-dwim-timeout 0.5)
  (defvar set-mark-dwim-repeat-action 'embark-act)
  (defvar set-mark-dwim-timeout-action 'completion-at-point)
  (defvar-keymap set-mark-dwim-map
    :doc "An briefly active keymap after set-mark-command"
    :repeat t
    "SPC" #'hippie-expand)
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
  (advice-add 'set-mark-command :around 'set-mark-dwim)
  (defvar M-O-cmd 'ff-find-other-file)
  (defun M-O-dwim ()
    (interactive)
    (if (sit-for 0.1)
        (call-interactively (setq this-command M-O-cmd))
      (set-transient-map xterm-function-map)
      (setq unread-command-events (append '(?\e ?O) unread-command-events))))
  (define-key global-map (kbd "M-O") 'M-O-dwim)
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
        (if (or (not window-system) (= 1 (length (frame-list))))
            (switch-to-buffer nil)
          (other-frame 1))
      (apply o args)))
  (advice-add 'other-window :around 'switch-to-last-buffer-if-one-window)
  (advice-add 'electric-pair-open-newline-between-pairs-psif
              :after (lambda ()
                       (when (eq last-command-event ?\n)
                         (indent-according-to-mode)))))
(use-package delight
  :config
  (delight '((auto-revert-mode "" autorevert)
             (eldoc-mode "" eldoc)
             (outline-minor-mode "" outline))))
(use-package avy
  :bind (("C-'" . avy-goto-char-timer) :map isearch-mode-map ("C-'" . avy-isearch))
  :config
  (defun avy-pop-mark-if-prefix (o &rest args)
    (if current-prefix-arg
        (call-interactively 'avy-pop-mark)
      (apply o args)))
  (advice-add 'avy-goto-char-timer :around 'avy-pop-mark-if-prefix))
(use-package browse-url
  :defer t
  :config
  (advice-add 'browse-url-default-browser :around 'browse-url-maybe-use-browser)
  (defun browse-url-maybe-use-browser (o &rest args)
    (let ((url (car args)))
      (if (getenv "BROWSER")
          (call-process (getenv "BROWSER") nil nil nil url)
        (apply o args)))))
(use-package cape
  :config
  (setq completion-at-point-functions
        (nconc completion-at-point-functions
               '(cape-history cape-file cape-keyword cape-dabbrev cape-elisp-block))))
(use-package clipetty
  :ensure
  :functions clipetty--emit
  :preface
  (advice-add 'browse-url-default-browser :around 'browse-url-maybe-use-clipetty)
  (defun browse-url-maybe-use-clipetty (o &rest args)
    (let ((url (car args)))
      (if (getenv "SSH_CLIENT")
          (clipetty--emit (concat "\e]1337;OpenURL=:" (base64-encode-string url) "\007"))
        (apply o args)))))
(use-package compile
  :bind (("M-C" . compile) ("M-R" . recompile))
  :custom
  (compilation-environment '("TERM=xterm-256color"))
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-buffer-name-function 'get-idle-compilation--buffer-name)
  (compilation-save-buffers-predicate 'ignore)
  :config
  (require 'eat nil t)
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
         ("C-c k"   . consult-kmacro)
         ("C-x M-:" . consult-complex-command)
         ("C-x b"   . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("C-h C-i"   . consult-info)
         ("C-h SPC"   . consult-mark)
         ("C-h C-SPC" . consult-global-mark)

         :map minibuffer-local-map
         ("M-r"   . consult-history)

         :map goto-map
         ("f"   . consult-flymake)

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
  (defun consult-imenu-across-all-buffers (o &rest args)
    (if current-prefix-arg
        (call-interactively 'consult-imenu-multi)
      (apply o args)))
  (advice-add 'consult-imenu :around 'consult-imenu-across-all-buffers))
(use-package completion-preview
  :delight
  :ensure nil
  :hook (prog-mode text-mode)
  :bind ( :map completion-preview-active-mode-map
          ("M-n" . completion-preview-next-candidate)
          ("M-p" . completion-preview-prev-candidate)))
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers--turn-on))
(use-package easy-kill
  :after embark
  :bind ([remap kill-ring-save] . easy-kill)
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
  :vc (:url "https://codeberg.org/akib/emacs-eat" :rev :newest)
  :bind (("C-z" . eat) :map eat-mode-map ("C-z" . eat-toggle-char-mode))
  :custom
  (eat-shell-prompt-annotation-position 'right-margin)
  :init
  (defun override-eat-term-keymap (map)
    (define-key map (kbd "M-\"") 'consult-register-load)
    (define-key map (kbd "C-z")  'eat-toggle-char-mode)
    map)
  (advice-add 'eat-term-make-keymap :filter-return 'override-eat-term-keymap)
  (defun eat-dwim (o &rest args)
    (if (or (not (called-interactively-p 'any)) (car args) (cadr args)
            (not (derived-mode-p 'eat-mode))
            (not (process-live-p (get-buffer-process (current-buffer)))))
        (apply o args)
      (bury-buffer)))
  (advice-add 'eat :around 'eat-dwim)
  (defun eat-toggle-char-mode ()
    (interactive)
    (call-interactively
     (cond ((or (null eat-terminal)
                current-prefix-arg)
            'eat)
           (eat--semi-char-mode
            'eat-emacs-mode)
           (t
            'eat-semi-char-mode))))
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
  (advice-add 'eat--pre-cmd :after 'eat-insert-invocation-time)
  (defun eat-insert-invocation-time ()
    (let* ((pos (pos-eol 0))
           (text (format-time-string "%m/%d %H:%M:%S"))
           (ov (make-overlay (1- pos) pos)))
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'after-string
                   (concat
                    (propertize " " 'display `(space :align-to (- right-fringe ,(1+ (length text)))))
                    (propertize text 'face '(italic font-lock-comment-face))))))
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
        (setq-local compilation-arguments (list command (if (eq mode 'compilation-minor-mode) nil mode) name-function))
        (setq-local revert-buffer-function 'compilation-revert-buffer)
        (setq next-error-last-buffer outbuf)
        (display-buffer outbuf '(nil (allow-no-window . t)))))))
(use-package ediff
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :bind (:map goto-map ("=" . ediff-current-file)))
(use-package embark
  :commands (embark-act embark-prefix-help-command)
  :functions embark--targets
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         :map minibuffer-local-map
             ("M-E" . embark-export)
             :map embark-region-map
         ("x" . shell-command)
         ("C" . compile)
         :map help-map
         ("b" . embark-bindings)
                 :map embark-file-map
                 ("." . open-file-in-vscode))
  :custom
  (prefix-help-command 'embark-prefix-help-command)
  :config
  (setq embark-indicators (delq 'embark-mixed-indicator embark-indicators))
  (add-to-list 'embark-indicators 'embark-minimal-indicator)
  (add-to-list 'embark-post-action-hooks '(kill-current-buffer embark--restart))
  (push 'embark--xref-push-marker (alist-get 'find-file embark-pre-action-hooks))
  (defun open-file-in-vscode (filename &optional _)
    (interactive
     (find-file-read-args "Find file: "
                          (confirm-nonexistent-file-or-buffer)))
    (call-process "code" nil nil nil (expand-file-name filename))))
(static-if window-system
    (use-package exec-path-from-shell
      :hook (after-init . exec-path-from-shell-initialize)))
(use-package go-ts-mode
  :hook ((go-ts-mode . eglot-ensure)
         (before-save . gofmt-before-save))
  :config
  (use-package go-mode :functions (gofmt) :custom (gofmt-command "goimports"))
  (defun gofmt-before-save () (when (derived-mode-p 'go-mode) (gofmt))))
(use-package hl-line :hook (prog-mode conf-mode compilation-mode eat-mode text-mode))
(use-package iedit :bind (("C-c E" . iedit-mode) :map isearch-mode-map ("M-e" . iedit-mode-from-isearch)))
(use-package marginalia
  :ensure
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :hook (after-init . marginalia-mode))
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism)))
(use-package org
  :bind (:map org-mode-map ("C-TAB" . nil) ("C-c ;" . nil))
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
  (use-package ob-compile :ensure nil)
  (defun lazy-load-org-babel-languages (o &rest args)
    (when-let* ((lang (org-element-property :language (org-element-at-point))))
      (when (or (string= lang "bash") (string= lang "sh")) (setq lang "shell"))
      (unless (cdr (assoc (intern lang) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern lang) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)))
    (apply o args))
  (advice-add 'org-babel-execute-src-block :around 'lazy-load-org-babel-languages))
(use-package outline-indent
  :delight outline-indent-minor-mode
  :bind (:map outline-indent-minor-mode-map ("S-<tab>" . outline-toggle-children))
  :hook (prog-mode . outline-indent-minor-mode))
(static-if window-system
    (progn
      (setq package-vc-allow-build-commands t)
      (use-package reader
        :disabled
        :vc (:url "https://codeberg.org/divyaranjan/emacs-reader"
                  :make "all"))
      (use-package pdf-tools
        :magic ("%PDF" . pdf-view-mode)
        :config
        (pdf-tools-install :no-query))))
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
(use-package smerge-mode
  :after embark
  :hook (find-file . smerge-start-session)
  :config
  (defvar embark-vc-conflict-map (make-composed-keymap smerge-basic-map embark-general-map))
  (defun embark-vc-target-conflict-at-point ()
    "Target a Merge Conflict at point."
    (when-let* ((d (save-match-data (and smerge-mode (smerge-match-conflict) (match-data 0)))))
      `(conflict "hunk" ,(car d) . ,(cadr d))))
  (add-to-list 'embark-target-finders 'embark-vc-target-conflict-at-point)
  (add-to-list 'embark-keymap-alist '(conflict . embark-vc-conflict-map)))
(static-if (and (fboundp 'treesit-available-p) (treesit-available-p))
    (use-package treesit-auto
      :hook ((after-init . global-treesit-auto-mode))
      :custom
      (treesit-font-lock-level 4)))
(use-package vertico
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :bind (("C-c C-r" . vertico-repeat)
         :map vertico-map
         ("M-E"   . embark-export)
         ("C-j"   . vertico-exit-input)
         ("DEL"   . vertico-directory-delete-char)
         ("M-/"   . (lambda () (interactive) (vertico-exit-and-run #'consult-find)))
         ("C-z"   . vertico-exit-and-run)
         ("M-s g" . vertico-exit-and-run)
         ("M-s r" . vertico-exit-and-run))
  :custom
  (vertico-count-format nil)
  :config
  (defun vertico-exit-and-run (&optional cmd)
    (interactive)
    (or cmd (setq cmd (lookup-key global-map (this-command-keys))))
    (vertico-insert)
    (run-at-time 0 nil
                 (lambda (d) (let ((default-directory d))
                               (call-interactively (setq this-command cmd))))
                 (file-name-directory (substitute-in-file-name (minibuffer-contents-no-properties))))
    (abort-recursive-edit)))
(use-package vterm
  :bind (("C-`" . vterm)
         :map vterm-mode-map
         ("C-q"  . vterm-send-next-key)
         ("C-z"  . nil)
         ("M-:"  . nil)
         ("M-\"" . nil))
  :custom
  (vterm-kill-buffer-on-exit nil))
(use-package vundo :bind ("C-x u" . vundo))
(use-package wgrep :custom (wgrep-auto-save-buffer t))
