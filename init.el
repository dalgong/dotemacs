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
  (dolist (p '(use-package diminish bind-key))
    (unless (require p nil t)
      (package-refresh-contents)
      (package-install p))))

(eval-when-compile
  (require 'use-package nil t)
  (require 'bind-key nil t))
(eval-and-compile
  (defmacro csetq (sym val)
    `(funcall (or (get ',sym 'custom-set) 'set-default) ',sym ,val))
  (defmacro use-package-when (pkg cond &rest body)
    (declare (indent 2))
    `(use-package ,pkg
       ,@(if (eval cond)
             body
           (cons ':disabled body)))))
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
  (backup-directory-alist '(("." . "~/.cache/emacs/backups")))
  (bidi-display-reordering 'left-to-right)
  (bidi-inhibit-bpa t)
  (bidi-paragraph-direction 'left-to-right)
  (blink-matching-paren t)
  (calc-display-trail nil)
  (completion-cycle-threshold 10)
  (create-lockfiles nil)
  (cursor-in-non-selected-windows nil)
  (delete-old-versions t)
  (disabled-command-function nil)
  (display-buffer-alist '(("\\*shell\\*" display-buffer-same-window)
                          ("\\*compilation\\*" display-buffer-in-bottom-window)))
  (enable-recursive-minibuffers t)
  (eval-expression-print-length nil)
  (eval-expression-print-level nil)
  (even-window-heights nil)
  (fast-but-imprecise-scrolling t)
  (ffap-machine-p-known 'reject)
  (fit-window-to-buffer-horizontally t)
  (font-lock-maximum-decoration '((c-mode . 2) (c++-mode . 2) (t . t)))
  (frame-inhibit-implied-resize t)
  (frame-resize-pixelwise t)
  (hscroll-margin 2)
  (hscroll-step 1)
  (help-char ?^)
  (history-length 1000)
  (highlight-nonselected-windows nil)
  (idle-update-delay 1)
  (indent-tabs-mode nil)
  (inhibit-compacting-font-caches t)
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
  (minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
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
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-process-output-max (* 1024 1024))
  (redisplay-skip-fontification-on-input t)
  (resize-mini-windows 'grow-only)
  (ring-bell-function 'ignore)
  (scroll-conservatively 101)
  (scroll-preserve-screen-position t)
  (scroll-margin 0)
  (scroll-step 1)
  (select-active-regions nil)
  (sentence-end-double-space nil)
  (set-mark-command-repeat-pop t)
  (shell-command-switch "-lc")
  (split-height-threshold nil)
  (split-width-threshold 160)
  (suggest-key-bindings nil)
  (tab-always-indent 'complete)
  (truncate-lines t)
  (use-dialog-box nil)
  (use-package-compute-statistics nil)
  (vc-follow-symlinks t)
  (version-control t)
  (visible-bell nil)
  (x-underline-at-descent-line t)
  (x-selection-timeout 100)
  (window-divider-default-places t)
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  (window-resize-pixelwise nil)
  :hook (after-init . find-function-setup-keys)
  :bind (([C-tab]              . other-window)
         ([C-S-tab]            . backward-other-window)
         ([C-up]               . windmove-up)
         ([C-down]             . windmove-down)
         ([C-left]             . windmove-left)
         ([C-right]            . windmove-right)
         ([remap suspend-frame]. ignore)
         ([remap kill-buffer]  . kill-this-buffer)
         ("C-TAB"              . other-window)
         ("C-S-TAB"            . backward-other-window)
         ("C-."                . next-error)
         ("C-,"                . previous-error)
         ("C-RET"              . open-dwim)
         ("M-K"                . kill-this-buffer)
         ("M-o"                . find-file)
         ("M-q"                . fill-paragraph)
         ("RET"                . newline-and-indent)
         ("M-9"                . open-dwim)
         ("<mouse-2>"          . mouse-run-command-dwim)
         ("<mouse-3>"          . mouse-open-dwim)

         ("M-n"                . forward-paragraph)
         ("M-p"                . backward-paragraph)

         :map help-map
         ("1"                  . byte-compile-file)
         ("2"                  . package-list-packages-no-fetch)
         ("3"                  . package-install)
         ("C-b"                . describe-personal-keybindings)
         ("="                  . quick-calc)

         :map ctl-x-map
         ("k"                  . kill-this-buffer)
         ("O"                  . ff-find-other-file)

         :map mode-specific-map
         ("SPC"                . cycle-spacing)
         ("C-_"                . recursive-edit)
         ("b"                  . bury-buffer)
         ("q"                  . delete-other-window)
         ("r"                  . replace-regexp)
         ("s"                  . replace-string)
         ("u"                  . rename-uniquely)
         ("D"                  . toggle-debug-on-error)
         ("E"                  . erase-buffer)
         ("x"                  . shell-command)
         ("X"                  . async-shell-command))
  :config
  (defun backward-other-window (count &optional all-frames interactive)
    (interactive "p\ni\np")
    (other-window (- count) all-frames interactive))
  (advice-add 'display-startup-echo-area-message :override #'ignore)
  (advice-add #'recursive-edit :around #'preseve-window-configuration-if-interactive)
  (advice-add 'split-window-right :after #'call-other-window-if-interactive)
  (advice-add 'split-window-below :after #'call-other-window-if-interactive)
  (defun call-other-window-if-interactive (&rest _)
    (when (called-interactively-p 'interactive)
      (other-window 1)))
  (defun preseve-window-configuration-if-interactive (o)
    (let ((wc (and (called-interactively-p 'interactive)
                   (current-window-configuration))))
      (unwind-protect (funcall o)
        (and wc (set-window-configuration wc)))))
  (defun delete-other-window ()
    (interactive)
    (unless (one-window-p 'nomini)
      (other-window 1)
      (if current-prefix-arg
          (progn (bury-buffer) (other-window -1))
        (delete-window (selected-window)))))
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
  (defun get-current-active-selection ()
    (let ((p (if (use-region-p)
                 (cons (region-beginning) (region-end))
               (and (fboundp 'easy-kill--bounds)
                    (ignore-errors (funcall 'easy-kill--bounds))))))
      (and p (car p) (buffer-substring-no-properties (car p) (cdr p))))))
(use-package files
  :custom
  (backup-by-copying-when-linked t)
  (backup-by-copying-when-mismatch t)
  (confirm-kill-emacs nil)
  (confirm-nonexistent-file-or-buffer nil)
  (require-final-newline t)
  (server-client-instructions nil)
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
(use-package ace-window
  :ensure
  :bind ("M-`" . ace-window))
(use-package auto-highlight-symbol
  :ensure
  :diminish
  :hook (after-init . global-auto-highlight-symbol-mode)
  :bind (:map auto-highlight-symbol-mode-map
              ("C-x '" . ahs-change-range)))
(use-package avy
  :ensure
  :bind (("C-'"   . avy-goto-char-timer)
         ("C-c '" . avy-goto-char-timer)
         ("M-g SPC" . avy-goto-char-timer)
         ("M-g M-SPC" . avy-goto-char-timer))
  :config
  (advice-add 'avy-goto-char-timer :around
              (defun avy-pop-mark-if-prefix (o &rest args)
                (if current-prefix-arg
                    (call-interactively 'avy-pop-mark)
                  (apply o args)))))
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
  (comint-prompt-read-only t)
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
         :map mode-specific-map
         ("SPC"   . company-manual-begin)
         ("C-SPC" . company-manual-begin)
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
  (advice-add 'set-mark-command :around #'company-complete-dwim)
  (defun company-complete-dwim (o &rest args)
    (if (or (not (called-interactively-p 'interactive))
            current-prefix-arg
            (memq last-command '(set-mark-command cua-set-mark pop-to-mark-command pop-global-mark))
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
         ("." . rename-uniquely)
         ("t" . toggle-truncate-lines)
         ([remap read-only-mode] . compilation-toggle-shell-mode))
  :custom
  (compilation-environment '("TERM=xterm-256color"))
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-save-buffers-predicate (lambda ()))
  (compilation-scroll-output 'first-error)
  (completion-auto-help 'lazy)
  (completion-pcm-complete-word-inserts-delimiters t)
  :config
  (defun apply-xterm-color-filter ()
    (let* ((proc (get-buffer-process (current-buffer)))
           (end-marker (and proc (process-mark proc))))
      (goto-char compilation-filter-start)
      (while (search-forward "\033[2K" end-marker t)
        (let ((p (point-at-bol))
              (cnt 1))
          (save-excursion
            (while (search-backward "\033[1A" p t)
              (cl-decf cnt))
            (setq p (point-at-bol cnt)))
          (setq compilation-filter-start (min compilation-filter-start p))
          (delete-region p (point))))
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
(use-package compiler-explorer
  :ensure
  :bind (:map help-map ("C" . compiler-explorer)))
(use-package dabbrev
  :bind (("C-M-_" . dabbrev-completion)
         ("C-M-/" . dabbrev-completion)
         :map mode-specific-map
         ("/" . dabbrev-expand))
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
  (dired-no-confirm t)
  (dired-use-ls-dired nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
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
(use-package dired-sidebar
  :disabled
  :ensure
  :bind ("C-x C-j" . dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-no-delete-other-windows t)
  (dired-sidebar-one-instance-p t)
  (dired-sidebar-should-follow-file t)
  (dired-sidebar-theme 'ascii))
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
(use-package elec-pair
  :hook (after-init . electric-pair-mode))
(use-package elfeed
  :bind (("C-x !" . elfeed)))
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
(use-package prescient
  :ensure
  :hook (after-init . prescient-persist-mode))
(use-package selectrum
  :ensure
  :bind ( :map help-map ("M-q" . selectrum-cycle-display-style)
          :map mode-specific-map ("C-r" . selectrum-repeat))
  :hook (after-init . selectrum-mode)
  :custom
  (selectrum-count-style nil)
  (file-name-shadow-properties '(invisible t))
  :config
  (use-package orderless
    :ensure
    :custom
    (completion-styles '(orderless))
    (orderless-skip-highlighting (lambda () selectrum-is-active))
    (selectrum-highlight-candidates-function #'orderless-highlight-matches))
  (use-package selectrum-prescient
    :ensure
    :custom
    (selectrum-prescient-enable-filtering nil)
    :config
    (selectrum-prescient-mode 1)))
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

         :map mode-specific-map
         ("h" . consult-history)
         ("b" . consult-bookmark)
         ("g" . consult-ripgrep)
         ("G" . consult-git-grep)
         ("C-g" . consult-grep)
         ("k" . consult-kmacro)
         ("m" . consult-mode-command)

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
         ("I" . consult-project-imenu)
         ("e" . consult-error)
         :map search-map
         ("f" . consult-find)
         ("g" . consult-grep)
         ("l" . consult-line)
         ("m" . consult-multi-occur)
         ("o" . consult-line-symbol-at-point)
         ("O" . consult-focus-lines-symbol-at-point)
         ("k" . consult-keep-lines)
         ("u" . consult-focus-lines)
         ("e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)
         ("M-l" . consult-line))
  :custom
  (register-preview-delay 0)
  (register-preview-function #'consult-register-format)
  ;; (consult-find-command "fd --color=never --full-path ARG OPTS")
  (consult-preview-key 'any)
  (consult-narrow-key (kbd "C-SPC"))
  :config
  ;; (nconc consult--source-bookmark (list :state #'consult--bookmark-preview))
  ;; (nconc consult--source-file (list :state #'consult--file-preview))
  ;; (nconc consult--source-project-file (list :state #'consult--file-preview))
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'consult-imenu :around
              (defun consult-imenu-across-all-buffers (o &rest args)
                (if current-prefix-arg
                    (let* ((buffers (cl-remove-if-not
                                     (lambda (b)
                                       (eq major-mode
                                           (buffer-local-value 'major-mode b)))
                                     (buffer-list))))
                      (consult--imenu (consult--imenu-all-items buffers)))
                  (apply o args))))
  (defun consult-line-symbol-at-point ()
    (interactive)
    (consult-line (thing-at-point 'symbol)))
  (defun consult-focus-lines-symbol-at-point ()
    (interactive)
    (consult-focus-lines
     nil
     (consult--completion-filter 'consult-location nil)
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
  (when (require 'selectrum nil t)
    (define-key selectrum-minibuffer-map (kbd "M-P") #'consult-toggle-preview)))
(use-package consult-flycheck
  :ensure
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))
(use-package marginalia
  :ensure
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators
   '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :hook (after-init . marginalia-mode)
  :config
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))
(use-package embark
  :ensure
  :after selectrum
  :bind (([remap just-one-space] . embark-act*)
         :map minibuffer-local-map
         ("M-SPC" . embark-act)
         :map selectrum-minibuffer-map
         ("M-SPC" . embark-act))
  :custom
  (embark-action-indicator
   (lambda (map _target)
     (which-key--show-keymap "Embark" map nil nil 'no-paging)
     #'which-key--hide-popup-ignore-command)
   embark-become-indicator embark-action-indicator)
  :config
  (defun embark-act-noquit ()
    "Run action but don't quit the minibuffer afterwards."
    (interactive)
    (let ((embark-quit-after-action nil))
      (embark-act)))
  (defun embark-act* ()
    (interactive)
    (call-interactively
     (if (car (embark--target)) 'embark-act 'just-one-space))))
(use-package embark-consult
  :ensure
  :after (embark consult)
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))
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
(use-package mb-depth
  :hook (after-init . minibuffer-depth-indicate-mode))
(use-package multiple-cursors
  :ensure
  :bind (:map mode-specific-map
              ("e" . mc/edit-lines)
              ("A" . mc/mark-all-in-region)))
(use-package org
  :bind (:map mode-specific-map
              ("l" . org-store-link)
              ("a" . org-agenda)
              ("c" . org-capture)
              ("C" . org-goto-calendar)
              :map org-mode-map
              ("C-TAB" . nil)
              ("C-c ;" . nil))
  :custom
  (org-agenda-span 'fortnight)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-odd-levels-only nil)
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  :config
  (use-package ob-async :ensure)
  (require 'org-tempo nil t)
  (let ((languages '((dot . t)
                     (emacs-lisp . t)
                     (shell . t))))
    (when (require 'ob-compile nil t)
      (define-key mode-specific-map "8" #'ob-compile)
      (push '(compile . t) languages))
    (org-babel-do-load-languages 'org-babel-load-languages languages)))
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
(use-package rtags
  :disabled
  :after company
  :custom
  (rtags-completions-enabled t)
  :config
  (add-to-list 'company-backends 'company-rtags)
  (rtags-enable-standard-keybindings)
  (use-package cmake-ide
    :ensure
    :config
    (cmake-ide-setup)))
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
  (put 'minibuffer-history                    'history-length 50)
  (put 'buffer-name-history                   'history-length 50)
  (put 'file-name-history                     'history-length 50))
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
(use-package shell-pop
  :ensure
  :bind ("C-`" . shell-pop)
  :custom
  (shell-pop-window-size 50)
  (shell-pop-full-span nil))
(use-package smerge-mode
  :custom
  (smerge-command-prefix "\C-z"))
(use-package smex
  :ensure
  :hook (after-init . smex-initialize))
(use-package so-long
  :hook (after-init . global-so-long-mode))
(use-package-when tab-bar (>= (string-to-number emacs-version) 27)
  :bind (("<C-prior>" . tab-previous)
         ("<C-next>"  . tab-next)
         :map tab-prefix-map
         ("O" . tab-previous)
         ("t" . tab-switcher))
  :custom
  (tab-bar-show nil))
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
(use-package vterm
  :disabled
  :config
  (csetq shell-pop-shell-type '("vterm" "*vterm*" (lambda () (vterm))))
  (advice-add #'vterm-yank-pop :override
              (defun use-consult-yank-pop (&optional arg)
                (interactive "p")
                (vterm-goto-char (point))
                (let ((inhibit-read-only t)
                      (yank-undo-function #'(lambda (_start _end) (vterm-undo))))
                  (cl-letf (((symbol-function 'insert-for-yank) #'vterm-insert))
                    (consult-yank-pop arg))))))
(use-package which-key
  :ensure
  :hook (after-init . which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 1)
  (which-key-popup-type 'minibuffer))
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
                                    xref-find-references))
  (xref-search-program 'ripgrep)
  (xref-show-xrefs-function 'xref-show-definitions-completing-read)
  (xref-show-definitions-function 'xref-show-definitions-completing-read))
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
