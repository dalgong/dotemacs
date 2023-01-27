;; -*- lexical-binding: t -*-
(setq custom-file "/dev/null")
(advice-add #'custom-save-all :override #'ignore)
(add-to-list 'load-path "~/.emacs.d/lisp")

(nconc after-init-hook
       '(delete-selection-mode electric-pair-mode
         global-reveal-mode minibuffer-depth-indicate-mode repeat-mode
         recentf-mode savehist-mode save-place-mode))

(when (require 'package nil t)
  (nconc package-archives '(("melpa"  . "http://melpa.org/packages/")
                            ("org"    . "http://orgmode.org/elpa/")))
  (require 'bind-key nil t)
  (require 'use-package nil t))

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
   ("C-."                . next-error)
   ("C-,"                . previous-error)
   ("M-o"                . other-window)
   ("RET"                . newline-and-indent)
   ("M-C"                . compile)
   ("M-D"                . recompile)
   ("M-H"                . ff-find-other-file)
   ("M-K"                . kill-this-buffer)
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
   ("q"                  . [C-tab 24 48])
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
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-no-confirm t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-switches-in-mode-line 'as-is)
  (dired-use-ls-dired nil)
  (disabled-command-function nil)
  (display-buffer-alist '(("\\*shell\\*" display-buffer-same-window)
                          ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                           display-buffer-at-bottom
                           (window-parameters (mode-line-format . none)))
                          ("\\*hermes.*" display-buffer-same-window)))
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
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (idle-update-delay 1)
  (indent-tabs-mode nil)
  (inhibit-compacting-font-caches t)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (inhibit-startup-echo-area-message (user-login-name))
  (isearch-allow-scroll 'unlimited)
  (isearch-yank-on-move 'shift)
  (isearch-lazy-count t)
  (isearch-repeat-on-direction-change t)
  (kill-buffer-query-functions nil)
  (kill-do-not-save-duplicates t)
  (kill-read-only-ok t)
  (kill-ring-max 3000)
  (kill-whole-line t)
  (lazy-highlight-buffer t)
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
  (proced-enable-color-flag t)
  (read-buffer-completion-ignore-case t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (read-file-name-completion-ignore-case t)
  (read-process-output-max (* 1024 1024))
  (recentf-auto-cleanup (* 3 3600))
  (recentf-max-saved-items 1000)
  (redisplay-skip-fontification-on-input t)
  (remote-file-name-inhibit-locks t)
  (require-final-newline t)
  (resize-mini-windows t)
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
  (tramp-auto-save-directory "~/.cache/emacs/backups")
  (tramp-persistency-file-name "~/.emacs.d/data/tramp")
  (tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root")))
  (truncate-lines t)
  (use-dialog-box nil)
  (use-package-compute-statistics nil)
  (use-package-enable-imenu-support t)
  (use-short-answers t)
  (vc-follow-symlinks nil)
  (version-control t)
  (view-read-only t)
  (visible-bell nil)
  (xref-prompt-for-identifier '(not xref-find-definitions
                                    xref-find-definitions-other-window
                                    xref-find-definitions-other-frame
                                    xref-find-references))
  (xref-search-program 'ripgrep)
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
  (dotimes (c 4)
    (global-set-key (kbd (format "M-%d" c)) (kbd (format "C-x %d" c))))
  (or standard-display-table (setq standard-display-table (make-display-table)))
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?┃))
  (put 'other-window 'repeat-map nil)
  (defvar set-mark-dwim-timeout 0.5)
  (defvar set-mark-dwim-repeat-action 'embark-act)
  (defvar set-mark-dwim-timeout-action 'company-indent-or-complete-common)
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
  (defun preseve-window-configuration-if-interactive (o)
    (let ((wc (and (called-interactively-p 'interactive)
                   (current-window-configuration))))
      (unwind-protect (funcall o)
        (and wc (set-window-configuration wc)))))
  (advice-add #'recursive-edit :around #'preseve-window-configuration-if-interactive)
  (defun call-other-window-if-interactive (&rest _)
    (when (called-interactively-p 'any)
      (other-window 1)))
  (advice-add 'split-window-right :after #'call-other-window-if-interactive)
  (advice-add 'split-window-below :after #'call-other-window-if-interactive)
  (defun switch-to-last-buffer-if-one-window (o &rest args)
    (if (and (one-window-p 'nomini) (called-interactively-p 'interactive))
        (switch-to-buffer nil)
      (apply o args)))
  (advice-add 'other-window :around #'switch-to-last-buffer-if-one-window)
  (defun get-current-active-selection ()
    (let ((p (if (use-region-p)
                 (cons (region-beginning) (region-end))
               (and (fboundp 'easy-kill--bounds)
                    (ignore-errors (funcall 'easy-kill--bounds))))))
      (and p (car p) (buffer-substring-no-properties (car p) (cdr p)))))
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
  (advice-add 'read-shell-command :around #'use-region-if-active)
  (advice-add #'electric-pair-open-newline-between-pairs-psif
              :after (lambda ()
                       (when (eq last-command-event ?\n)
                         (indent-according-to-mode))))
  (add-hook 'next-error-hook 'reposition-window)
  (add-hook 'find-function-after-hook 'reposition-window)
  (add-hook 'xref-after-return-hook 'reposition-window)
  (add-hook 'xref-after-jump-hook 'reposition-window)
  (remove-hook 'xref-after-jump-hook 'recenter)
  (add-hook 'outline-mode-hook
            (lambda ()
              (setq-local beginning-of-defun-function
                          (lambda () (outline-previous-visible-heading 1)))
              (setq-local end-of-defun-function
                          (lambda () (outline-next-visible-heading 1))))))
(use-package ace-window :ensure :bind ("C-x o" . ace-window))
(use-package amx        :ensure :hook after-init)
(use-package auto-highlight-symbol
  :ensure
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
(use-package company
  :ensure
  :hook (prog-mode text-mode)
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
  :bind (("<f7>" . compile)
         ("<f8>" . recompile)
         :map compilation-mode-map
         ([remap read-only-mode] . compilation-toggle-shell-mode))
  :custom
  (compilation-environment '("TERM=xterm-256color"))
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-buffer-name-function #'get-idle-compilation--buffer-name)
  (compilation-save-buffers-predicate (lambda ()))
  (compilation-scroll-output 'first-error)
  :config
  (defvar-local bpo-queue nil)
  (defvar-local bpo-queue-timer nil)
  (defun bpo-flush (proc)
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (when bpo-queue-timer
          (cancel-timer bpo-queue-timer)
          (setq bpo-queue-timer nil))
        (let ((inhibit-quit t)
              (inhibit-read-only t)
              (inhibit-modification-hooks t)
              (queue bpo-queue))
          (setq bpo-queue nil)
          ;; By the time delayed filter is called, process may be dead.
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (&rest _) proc)))
            (dolist (p (nreverse queue))
              (funcall (car p) proc (cdr p))))))))
  (defun bpo-enqueue (ofun proc o)
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (push (cons ofun o) bpo-queue)
        (unless bpo-queue-timer
          (setq bpo-queue-timer
                (run-with-timer 0.2 nil #'bpo-flush proc))))))
  (defun handle-process-buffered (proc)
    (with-current-buffer (process-buffer proc)
      (setq-local bpo-queue nil)
      (setq-local bpo-queue-timer nil))
    (add-function :around (process-filter proc) #'bpo-enqueue)
    (add-function :around (process-sentinel proc) #'bpo-enqueue))
  (if nil
      (progn
        (add-hook 'compilation-start-hook #'enable-coterm-on-compilation)
        (defun enable-coterm-on-compilation (proc)
          (with-current-buffer (process-buffer proc)
            (buffer-disable-undo)
            (coterm--init)
            (coterm-auto-char-mode -1)
            (coterm-auto-char-lighter-mode -1)
            (handle-process-buffered proc)
            (setq-local comint-input-ring compile-history)
            (setq-local comint-output-filter-functions '(ansi-color-process-output))
            (use-local-map compilation-mode-map)
            (setq-local jit-lock-defer-time nil)
            (setq buffer-read-only t)))
        (defun use-comint-always (args)
          (cl-list* (car args) t (cddr args)))
        (advice-add #'compilation-start :filter-args #'use-comint-always))
    (add-hook 'compilation-start-hook #'handle-process-buffered)
    (add-hook 'compilation-filter-hook #'apply-xterm-color-filter)
    (use-package xterm-color :ensure :functions xterm-color-filter)
    (defun apply-xterm-color-filter ()
      (let* ((proc (get-buffer-process (current-buffer)))
             (end-marker (and proc (process-mark proc)))
             (inhibit-redisplay t))
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
          (set-marker end-marker (point))))))
  (defun do-kill-compilation (o &rest args)
    (when (and (called-interactively-p 'interactive)
               (memq major-mode '(comint-mode compilation-mode))
               (get-buffer-process (current-buffer)))
      (kill-compilation))
    (apply o args))
  (advice-add #'recompile :around #'do-kill-compilation)
  (defun kill-compilation-for-comint (o &rest args)
    (interactive)
    (let* ((buffer (compilation-find-buffer))
           (proc (get-buffer-process buffer)))
      (if (and proc (eq 'comint-mode
                        (with-current-buffer buffer major-mode)))
          (comint-send-string proc (kbd "C-c"))
        (apply o args))))
  (advice-add #'kill-compilation :around #'kill-compilation-for-comint)
  (defun get-idle-compilation--buffer-name (name-of-mode)
    (let ((name (compilation--default-buffer-name name-of-mode)))
      (if (and (get-buffer name)
               (get-buffer-process (get-buffer name))
               (process-live-p (get-buffer-process (get-buffer name))))
          (let ((mode (buffer-local-value 'major-mode (get-buffer name))))
            (or (cl-loop for b in (buffer-list)
                         with name-re = (concat "^" (regexp-quote name))
                         when (and (string-match name-re (buffer-name b))
                                   (eq mode (buffer-local-value 'major-mode b))
                                   (not (and (get-buffer-process b)
                                             (process-live-p (get-buffer-process b)))))
                         return (buffer-name b))
                (generate-new-buffer-name name)))
        name)))
  (defun shell-toggle-compile-mode ()
    (interactive)
    (setq buffer-read-only t)
    (compilation-mode))
  (defun compilation-toggle-shell-mode ()
    (interactive)
    (setq buffer-read-only nil)
    (shell-mode)
    (bind-key [remap read-only-mode] #'shell-toggle-compile-mode (current-local-map))))
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
         ("o" . consult-line)
         ("l" . consult-line)
         ("L" . consult-line-multi)
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
  (defvar string-width #'string-width nil)
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
  (defvar-local consult-toggle-preview-orig nil)
  (defun consult-toggle-preview ()
    "Command to enable/disable preview."
    (interactive)
    (if consult-toggle-preview-orig
        (setq consult--preview-function consult-toggle-preview-orig
              consult-toggle-preview-orig nil)
      (setq consult-toggle-preview-orig consult--preview-function
            consult--preview-function #'ignore))))
(use-package coterm :ensure :hook after-init)
(use-package diffview
  :ensure
  :after diff-mode
  :bind (:map diff-mode-map ("|" . diffview-current)))
(use-package dired-subtree :ensure :after dired :bind (:map dired-mode-map ("TAB" . dired-subtree-toggle)))
(use-package dired-x               :after dired :defer t)
(use-package display-line-numbers  :hook (prog-mode text-mode))
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
  (ediff-split-window-function #'split-window-horizontally)
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  :config
  (defun do-without-ask (o &rest args)
    (cl-letf (((symbol-function 'y-or-n-p) #'(lambda (_) t)))
      (apply o args)))
  (advice-add #'ediff-janitor :around #'do-without-ask)
  (advice-add #'ediff-quit :around #'do-without-ask))
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
  (add-to-list 'embark-post-action-hooks '(kill-this-buffer embark--restart))
  (push #'embark--xref-push-marker (alist-get 'find-file embark-pre-action-hooks)))
(use-package embark-consult :ensure :hook (embark-collect-mode . consult-preview-at-point-mode))
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure
  :hook (after-init . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments '("-l")))
(use-package gcmh    :ensure :hook after-init)
(use-package go-mode :ensure :hook (go-mode . eglot-ensure))
(when (file-directory-p "~/work/hermes")
  (use-package hermes  :load-path "~/work/hermes" :commands hermes))
(use-package hl-line :hook (prog-mode conf-mode compilation-mode text-mode))
(use-package ibuffer :bind ([remap list-buffers] . ibuffer))
(use-package iedit   :ensure :bind (("C-c E" . iedit-mode) ("M-s E" . iedit-mode-from-isearch)))
(use-package isearch
  :hook (isearch-mode . search-for-region)
  :config
  (defun search-for-region ()
    (when-let (s (get-current-active-selection))
      (isearch-yank-string s)
      (deactivate-mark)
      (isearch-repeat (if isearch-forward 'forward 'backward)))))
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
  ;;   (require 'ob-compile nil t)
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
  (use-package ob-async   :ensure)
  (use-package ob-compile :bind (:map mode-specific-map ("8" . ob-compile)))
  (defun lazy-load-org-babel-languages (o &rest args)
    (when-let (lang (org-element-property :language (org-element-at-point)))
      (when (or (string= lang "bash") (string= lang "sh")) (setq lang "shell"))
      (unless (cdr (assoc (intern lang) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern lang) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)))
    (apply o args))
  (advice-add 'org-babel-execute-src-block :around #'lazy-load-org-babel-languages)
  (defun fix-missing-args (o &rest args)
    (when (> (length args) 4)
      (setf (nthcdr 4 args) nil))
    (apply o args))
  (advice-add #'ob-async-org-babel-execute-src-block :around #'fix-missing-args))
(use-package outline-magic :ensure :bind (("<backtab>" . outline-cycle)))
(use-package rustic
  :ensure
  :hook (rustic-mode . eglot-ensure)
  :bind ( :map rustic-mode-map
          ("C-c n" . flymake-goto-next-error)
          ("C-c p" . flymake-goto-prev-error))
  :custom
  (rustic-lsp-client 'eglot))
(use-package pdf-tools
  :ensure
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))
(use-package shell
  :bind (("C-`" . shell)
         :map comint-mode-map
         ([C-up]   . nil)
         ([C-down] . nil)
         :map shell-mode-map
         ("C-z" . comint-stop-subjob)
         ("M-." . comint-insert-previous-argument))
  :custom
  (comint-input-ignoredups t)
  :config
  (add-hook 'comint-output-filter-functions #'comint-osc-process-output)
  (add-hook 'comint-input-filter-functions #'show-prompt-time)
  (defun show-prompt-time (input)
    (unless (string-match "^[ \t\n\r]+$" input)
      (let ((s (format-time-string "%m/%d %T"))
            (ov (make-overlay (point-at-eol 0) (point-at-eol 0))))
        (overlay-put ov 'after-string
                     (concat
                      (propertize " "
                                  'display
                                  `(space :align-to (- right-fringe ,(+ 1 (length s)))))
                      (propertize s 'face 'font-lock-doc-face)))))))
(use-package smerge-mode
  :defer t
  :config
  (when (require 'transient nil t)
    (transient-define-prefix smerge-dispatch ()
      "Invoke an SMerge command from a list of available commands."
      [["Keep"
        ("b" "Base" smerge-keep-base)
        ("u" "Upper" smerge-keep-upper)
        ("l" "Lower" smerge-keep-lower)
        ("a" "All" smerge-keep-all) ("RET" "Current" smerge-keep-current)]
       ["Diff"
        ("<" "Base/upper" smerge-diff-base-upper)
        ("=" "Upper/lower" smerge-diff-upper-lower)
        (">" "Base/lower" smerge-diff-base-lower)
        ("R" "Refine" smerge-refine :transient t)]
       ["Other"
        ("C" "Combine" smerge-combine-with-next)
        ("r" "Resolve" smerge-resolve) ("x" "Kill current" smerge-kill-current)]])
    (define-key (plist-get smerge-text-properties 'keymap)
                (kbd "RET") 'smerge-dispatch)))
(if (and (fboundp 'treesit-available-p) (treesit-available-p))
    (use-package treesit
      :config
      (dolist (p '((c c-mode . c-ts-mode)
                   (cpp c++-mode . c++-ts-mode)
                   (python python-mode . python-ts-mode)
                   (bash sh-mode . bash-ts-mode)
                   (yaml yaml-mode . yaml-ts-mode)
                   (toml conf-toml-mode . toml-ts-mode)))
        (when (treesit-ready-p (car p))
          (add-to-list 'major-mode-remap-alist (cdr p)))))
  (use-package tree-sitter
    :ensure
    :hook ((tree-sitter-after-on . tree-sitter-hl-mode)
           ((rustic-mode c-mode-common) . tree-sitter-mode)))
  (use-package tree-sitter-langs :ensure :after tree-sitter))
(use-package vertico
  :ensure t
  :hook after-init
  :bind ( :map vertico-map
          ("?"       . minibuffer-completion-help)
          ("C-j"     . vertico-exit-input)
          ("DEL"     . vertico-directory-delete-char)
          ("M-DEL"   . vertico-directory-delete-word)
          ("M-P"     . consult-toggle-preview)
          ("M-/"     . consult-dir-jump-file)
          ("C-c SPC" . vertico-restrict-to-matches)
          :map mode-specific-map
          ("C-r"     . vertico-repeat))
  :custom
  (vertico-count-format nil)
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (defun vertico-restrict-to-matches ()
    (interactive)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert " ")
      (add-text-properties (minibuffer-prompt-end) (point-max)
                           '(invisible t read-only t cursor-intangible t rear-nonsticky t))))
  (use-package consult-dir :ensure :config (advice-add #'consult-dir-jump-file :before #'vertico-insert)))
(use-package wgrep :ensure)
