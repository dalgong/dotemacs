;; -*- lexical-binding: t -*-
(setq custom-file "/dev/null")
(advice-add #'custom-save-all :override #'ignore)
(add-to-list 'load-path "~/.emacs.d/lisp")

(advice-add #'tty-run-terminal-initialization :override #'ignore)
(add-hook 'window-setup-hook
          (defun restore-tty-run-terminal-initialization ()
            (advice-remove #'tty-run-terminal-initialization #'ignore)
            (unless (display-graphic-p)
              (tty-run-terminal-initialization (selected-frame) nil t))))

(when (require 'package nil t)
  (custom-set-variables
   '(package-archives
     '(("gnu"    . "http://elpa.gnu.org/packages/")
       ("nongnu" . "https://elpa.nongnu.org/nongnu/")
       ("melpa"  . "http://melpa.org/packages/")
       ("org"    . "http://orgmode.org/elpa/"))))
  (dolist (p '(use-package diminish bind-key))
    (unless (require p nil t)
      (package-refresh-contents)
      (package-install p))))

(eval-when-compile
  (require 'use-package nil t)
  (require 'bind-key nil t))

(use-package emacs
  :bind
  (([C-tab]              . other-window)
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
   ("M-o"                . other-window)
   ("M-q"                . fill-paragraph)
   ("RET"                . newline-and-indent)
   ("M-C"                . compile)
   ("M-D"                . recompile)
   ("M-n"                . forward-paragraph)
   ("M-p"                . backward-paragraph)

   :map help-map
   ("D"                  . toggle-debug-on-error)
   ("E"                  . erase-buffer)
   ("p"                  . package-list-packages-no-fetch)
   ("C-b"                . describe-personal-keybindings)
   ("C-o"                . proced)
   ("="                  . quick-calc)

   :map ctl-x-map
   ("O"                  . ff-find-other-file)

   :map mode-specific-map
   ("0"                  . recursive-edit)
   ("SPC"                . cycle-spacing)
   ("r"                  . replace-regexp)
   ("s"                  . replace-string))
 
  :custom
  (ad-redefinition-action 'accept)
  (async-shell-command-buffer 'rename-buffer)
  (async-shell-command-display-buffer nil)
  (auto-save-default nil)
  (auto-save-interval 0)
  (auto-window-vscroll nil)
  (backup-by-copying t)
  (backup-by-copying-when-linked t)
  (backup-by-copying-when-mismatch t)
  (backup-directory-alist '(("." . "~/.cache/emacs/backups")))
  (bidi-display-reordering 'left-to-right)
  (bidi-inhibit-bpa t)
  (bidi-paragraph-direction 'left-to-right)
  (blink-matching-paren t)
  (calc-display-trail nil)
  (column-number-indicator-zero-based nil)
  (completion-auto-help 'lazy)
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-cycle-threshold 1)
  (completion-ignore-case t)
  (completion-pcm-complete-word-inserts-delimiters t)
  (completion-show-help nil)
  (completions-format 'one-column)
  (completions-header-format nil)
  (completions-max-height 20)
  (completion-auto-select nil)
  (confirm-kill-emacs nil)
  (confirm-nonexistent-file-or-buffer nil)
  (create-lockfiles nil)
  (cursor-in-non-selected-windows nil)
  (delete-old-versions t)
  (dabbrev-case-fold-search nil)
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  (dired-switches-in-mode-line 'as-is)
  (disabled-command-function nil)
  (display-buffer-alist '(("\\*shell\\*" display-buffer-same-window)))
  (enable-recursive-minibuffers t)
  (eval-expression-print-length nil)
  (eval-expression-print-level nil)
  (even-window-heights nil)
  (evil-default-state 'emacs)
  (fast-but-imprecise-scrolling t)
  (ffap-machine-p-known 'reject)
  (fit-window-to-buffer-horizontally t)
  (font-lock-maximum-decoration '((c-mode . 2) (c++-mode . 2) (t . t)))
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
  (hscroll-margin 2)
  (hscroll-step 1)
  (help-char ?^)
  (help-window-select t)
  (history-delete-duplicates t)
  (history-length 1000)
  (highlight-nonselected-windows nil)
  (find-file-visit-truename nil)
  (idle-update-delay 1)
  (indent-tabs-mode nil)
  (inhibit-compacting-font-caches t)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (isearch-repeat-on-direction-change t)
  (kill-buffer-query-functions nil)
  (kill-do-not-save-duplicates t)
  (kill-read-only-ok t)
  (kill-ring-max 3000)
  (kill-whole-line t)
  (line-move-visual nil)
  (mac-option-key-is-meta t)
  (mac-right-option-modifier nil)
  (make-backup-files nil)
  (mark-even-if-inactive t)
  (max-mini-window-height 0.15)
  (minibuffer-eldef-shorten-default t)
  (minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (mode-line-client '(""))
  (mode-line-modified '("%* "))
  (mode-line-remote '(""))
  (mode-line-frame-identification '(""))
  (mode-line-front-space '("  "))
  (mode-line-position '(""))
  (mode-line-mule-info '(""))
  (mode-line-end-spaces nil)
  ;; '(:eval (concat (propertize " " 'display `(space :align-to (- right 4))) "%c")))
  (next-error-message-highlight t)
  (ns-alternate-modifier 'super)
  (ns-command-modifier 'meta)
  (ns-tool-bar-display-mode 'both)
  (ns-tool-bar-size-mode 'regular)
  (ns-use-thin-smoothing t)
  (read-buffer-completion-ignore-case t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (read-file-name-completion-ignore-case t)
  (read-process-output-max (* 1024 1024))
  (redisplay-skip-fontification-on-input t)
  (remote-file-name-inhibit-locks t)
  (require-final-newline t)
  (resize-mini-windows 'grow-only)
  (revert-without-query '(""))
  (ring-bell-function 'ignore)
  (save-interprogram-paste-before-kill t)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (scroll-margin 0)
  (scroll-step 1)
  (select-active-regions nil)
  (server-client-instructions nil)
  (sentence-end-double-space nil)
  (set-mark-command-repeat-pop t)
  (shell-command-switch "-lc")
  (shell-command-default-error-buffer "*Shell Command Errors*")
  (show-trailing-whitespace nil)
  (split-height-threshold nil)
  (split-width-threshold 160)
  (suggest-key-bindings nil)
  (tab-always-indent 'complete)
  (truncate-lines t)
  (use-dialog-box nil)
  (use-package-compute-statistics nil)
  (use-package-enable-imenu-support t)
  (use-short-answers t)
  (vc-follow-symlinks nil)
  (version-control t)
  (view-read-only t)
  (visible-bell nil)
  (x-underline-at-descent-line t)
  (x-selection-timeout 100)
  (y-or-n-p-use-read-key t)
  (warning-minimum-level :emergency)
  (window-divider-default-places 'right-only)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  (window-resize-pixelwise nil)
  (words-include-escapes t)
  :config
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
  (advice-add #'recursive-edit :around
              (defun preseve-window-configuration-if-interactive (o)
                (let ((wc (and (called-interactively-p 'interactive)
                               (current-window-configuration))))
                  (unwind-protect (funcall o)
                    (and wc (set-window-configuration wc))))))
  (defun get-current-active-selection ()
    (let ((p (if (use-region-p)
                 (cons (region-beginning) (region-end))
               (and (fboundp 'easy-kill--bounds)
                    (ignore-errors (funcall 'easy-kill--bounds))))))
      (and p (car p) (buffer-substring-no-properties (car p) (cdr p))))))
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
(use-package cc-mode
  :custom
  (c-electric-flag nil)
  :hook (c-mode-common . set-outline-regexp)
  :config
  (defun set-outline-regexp ()
    (require 'outline)
    (setq outline-regexp "\\s-*\\S-")))
(use-package comint
  :bind (:map comint-mode-map
              ([C-up]   . nil)
              ([C-down] . nil))
  :hook (comint-output-filter-functions . (comint-watch-for-password-prompt comint-truncate-buffer))
  :custom
  (comint-buffer-maximum-size 10240)
  (comint-move-point-for-output nil)
  (comint-scroll-to-bottom-on-input nil))
(use-package company
  :ensure
  :diminish
  :hook (after-init . global-company-mode)
  :bind ( :map prog-mode-map
          ("C-i"   . company-indent-or-complete-common)
          ([remap c-indent-line-or-region] . company-indent-or-complete-common)
          :map mode-specific-map
          ("/" . company-manual-begin)
          :map company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("M-s" . company-filter-candidates))
  :custom
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
         ("M-Y" . consult-imenu-multi)

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

         :map ctl-x-map
         ("M-:" . consult-complex-command)
         ("b"   . consult-buffer)
         ("C-r" . consult-recent-file)
         ("m"   . consult-mode-command)
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
    (if (not (eq 'buffer (plist-get (and (minibufferp) (car (embark--targets))) :type)))
        (apply o args)
      (setq unread-command-events (listify-key-sequence "k"))
      (call-interactively 'embark-act)))
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
  (advice-add #'find-file :around #'find-file--line-number)  
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
          ;; goto-line is for interactive use
          (goto-char (point-min))
          (forward-line (1- line-number))))))
  (advice-add #'ffap-file-at-point :filter-return #'ffap-file-at-point-add-line-number)
  (defun ffap-file-at-point-add-line-number (r)
    (let ((s (ffap-string-at-point)))
      (save-match-data
        (if (string-match "\\(:[0-9]+\\)\\(:[0-9]+\\)?$" s)
            (concat r (match-string 1 s))
          r))))
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
        (plist-put consult--source-buffer :state #'consult-buffer-state-no-tramp))
  (use-package consult-flycheck
    :ensure
    :bind (:map flycheck-command-map
                ("!" . consult-flycheck))))
(use-package coterm :ensure :hook (after-init . coterm-mode))
(use-package delsel         :hook (after-init . delete-selection-mode))
(use-package diffview
  :ensure
  :after diff-mode
  :bind (:map diff-mode-map ("|" . diffview-current)))
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
(use-package dired-subtree :ensure :after dired :bind (:map dired-mode-map ("TAB" . dired-subtree-toggle)))
(use-package dired-x               :after dired :hook (dired-mode . dired-extra-startup))
(use-package display-line-numbers  :hook ((prog-mode text-mode) . display-line-numbers-mode))
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
(use-package eat
  :ensure
  :hook (after-init . eat-eshell-mode)
  :custom
  (eshell-visual-commands nil))
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
  (advice-add #'ediff-janitor :filter-args (defun dont-ask (args) (setcar args nil) args))
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
(use-package elec-pair
  :hook (after-init . electric-pair-mode)
  :config
  (advice-add #'electric-pair-open-newline-between-pairs-psif
              :override
              (defun electric-pair-open-newline-between-pairs-psif-fix ()
                (when (and (if (functionp electric-pair-open-newline-between-pairs)
                               (funcall electric-pair-open-newline-between-pairs)
                             electric-pair-open-newline-between-pairs)
                           (eq last-command-event ?\n)
                           (< (1+ (point-min)) (point) (point-max))
                           (eq (save-excursion
                                 (skip-chars-backward "\t\s")
                                 (char-before (1- (point))))
                               (matching-paren (char-after))))
                  (save-excursion
                    (newline 1 t)
                    ;; this is missing
                    (indent-according-to-mode))))))
(use-package embark
  :ensure
  :commands (embark-act embark-prefix-help-command)
  :bind (("M-SPC" . embark-act)
         ("M-."   . embark-dwim)
         :map minibuffer-local-map
         ("M-E"   . embark-export)
         ("M-L"   . embark-live)
         ("M-S"   . embark-collect))
  :custom
  (embark-cycle-key (kbd "C-SPC"))
  (prefix-help-command #'embark-prefix-help-command)
  (embark-help-key "?")
  (embark-quit-after-action nil)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 display-buffer-at-bottom
                 (window-parameters (mode-line-format . none))))
  (add-to-list 'embark-post-action-hooks '(kill-this-buffer embark--restart))
  (push #'embark--xref-push-marker
        (alist-get 'find-file embark-pre-action-hooks)))
(use-package embark-consult
  :ensure
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure
  :hook (after-init . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments '("-l")))
(use-package gcmh :ensure :diminish :hook (after-init . gcmh-mode))
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
(use-package hermes
  :load-path "~/work/hermes"
  :bind (:map mode-specific-map ("v" . hermes))
  :config
  (add-to-list 'display-buffer-alist '("\\*hermes.*" display-buffer-same-window)))
(use-package hl-line :hook ((prog-mode conf-mode compilation-mode) . hl-line-mode))
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
(use-package marginalia
  :ensure
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :hook (after-init . marginalia-mode)
  :config
  (advice-add #'marginalia--buffer-file :around
              (lambda (o &rest args)
                (let ((original (symbol-function #'abbreviate-file-name)))
                  (cl-letf (((symbol-function #'abbreviate-file-name)
                             (lambda (filename)
                               (if (file-remote-p filename)
                                   filename
                                 (funcall original filename)))))
                    (apply o args))))))
(use-package mb-depth :hook (after-init . minibuffer-depth-indicate-mode))
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
  (defvar my-emacs-lisp-params nil)
  (advice-add #'org-babel-execute:emacs-lisp :around 
    (defun save-my-emacs-lisp-params (o body params)
      (funcall o body (setq my-emacs-lisp-params params))))
  ;; #+NAME: embed
  ;; #+BEGIN_SRC elisp :var block-name="" :var datum="" :var info="" :var lang="" :var body="" :exports none
  ;;   (save-excursion
  ;;     (org-babel-goto-named-src-block block-name)
  ;;     (setq datum (org-element-at-point))
  ;;     t)
  ;;   (setq info (org-babel-get-src-block-info nil datum))
  ;;   (cl-callf org-babel-merge-params my-emacs-lisp-params)
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
  (recentf-max-saved-items 1000))
(use-package repeat    :hook (after-init . repeat-mode))
(use-package rustic
  :ensure
  :custom
  (rustic-lsp-client 'eglot)
  :hook (rustic-mode . eglot-ensure))
(use-package reveal    :hook (after-init . global-reveal-mode))
(use-package savehist  :hook (after-init . savehist-mode))
(use-package saveplace :hook (after-init . save-place-mode))
(use-package shell
  :bind (:map shell-mode-map ([remap read-only-mode] . shell-toggle-compile-mode))
  :config
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output)
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
(use-package smerge-mode :custom (smerge-command-prefix "\C-cm"))
(use-package tempel
  :ensure
  :hook ((prog-mode text-mode org-mode) . tempel-setup-capf)
  :config
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (use-package tempel-collection
    :ensure t))
(use-package tramp
  :defer t
  :custom
  (tramp-auto-save-directory "~/.cache/emacs/backups")
  (tramp-persistency-file-name "~/.emacs.d/data/tramp")
  (tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root")))
  :config
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (add-to-list 'tramp-remote-path "~/bin"))
(use-package tree-sitter
  :ensure
  :diminish
  :hook ((tree-sitter-after-on . tree-sitter-hl-mode)
         ((rustic-mode c-mode-common) . tree-sitter-mode))
  :config
  (use-package tree-sitter-langs :ensure))
(use-package vertico
  :ensure t
  :bind ( :map vertico-map
          ("?" . minibuffer-completion-help)
          ("C-j" . vertico-exit-input)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word)
          ("M-'" . vertico-quick-insert)
          ("M-m" . vertico-quick-exit)
          ("C-x C-d" . consult-dir)
          ("M-/" . consult-dir-jump-file)
          ("C-c SPC" . +vertico-restrict-to-matches)
          :map mode-specific-map
          ("C-r" . vertico-repeat))
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save)
         (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :custom
  (vertico-count-format nil)
  :config
  (require 'vertico-repeat)
  (require 'vertico-directory)
  (defun +vertico-restrict-to-matches ()
    (interactive)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert " ")
      (add-text-properties (minibuffer-prompt-end) (point-max)
                           '(invisible t read-only t cursor-intangible t rear-nonsticky t))))
  (use-package consult-dir
    :ensure t
    :config
    (advice-add #'consult-dir-jump-file :before #'vertico-insert)))
(use-package vundo
  :ensure
  :bind ("C-x u" . vundo))
(use-package wgrep
  :ensure
  :hook (grep-setup . wgrep-setup)
  :bind (:map grep-mode-map ("e" . wgrep-change-to-wgrep-mode)))
(use-package xref
  :commands (xref-find-definitions xref-find-references)
  :custom
  (xref-prompt-for-identifier '(not xref-find-definitions
                                    xref-find-definitions-other-window
                                    xref-find-definitions-other-frame
                                    xref-find-references))
  (xref-search-program 'ripgrep))
