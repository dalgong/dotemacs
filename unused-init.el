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
(use-package affe
  :disabled
  :ensure
  :after orderless
  :bind ( :map mode-specific-map
          ("C-g" . affe-grep)
          :map search-map
          ("f" . affe-find)
          ("g" . affe-grep))
  :config
  ;; Configure Orderless
  (setq affe-regexp-function #'orderless-pattern-compiler
        affe-highlight-function #'orderless-highlight-matches)

  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))
(use-package cc-mode
  :custom
  (c-electric-flag nil)
  :hook (c-mode-common . set-outline-regexp)
  :config
  (defun set-outline-regexp ()
    (require 'outline)
    (setq outline-regexp "\\s-*\\S-")))
(use-package dired-sidebar
  :disabled
  :ensure
  :bind ("C-x C-j" . dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-no-delete-other-windows t)
  (dired-sidebar-one-instance-p t)
  (dired-sidebar-should-follow-file t)
  (dired-sidebar-theme 'ascii))
(use-package easy-repeat
  :disabled
  :ensure
  :hook (after-init . easy-repeat-mode)
  :config
  (dolist (c '(goto-last-change tab-next tab-previous tab-move))
    (cl-pushnew c easy-repeat-command-list)))
(use-package eat
  :ensure
  :hook (after-init . eat-eshell-mode)
  :bind ("M-`" . eat)
  :custom
  (eshell-visual-commands nil)
  :config
  (defun eat-shell (o &rest args)
    (interactive (list (or explicit-shell-file-name shell-file-name) current-prefix-arg))
    (apply o args))
  (advice-add #'eat :around #'eat-shell)
  (defun handle-eat-paste (o &rest args)
    (if eat--terminal
        (cl-letf ((symbol-function 'yank) (symbol-function 'eat-yank))
          (apply o args))
      (apply o args)))
  (advice-add #'xterm-paste :around #'handle-eat-paste))
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
(use-package so-long
  :disabled
  :hook (after-init . global-so-long-mode))
(use-package tab-bar
  :disabled
  :bind (("<C-prior>" . tab-previous)
         ("<C-next>"  . tab-next)
         :map tab-prefix-map
         ("O" . tab-previous)
         ("t" . tab-switcher))
  :custom
  (tab-bar-show nil))
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
(use-package icomplete
  :hook (after-init . icomplete-vertical-mode)
  :bind ( :map icomplete-minibuffer-map
          ("SPC" . self-insert-command)
          ("C-j" . icomplete-fido-exit)
          ("RET" . icomplete-force-complete-and-exit)
          ("<remap> <minibuffer-complete-and-exit>" . icomplete-force-complete-and-exit))
  :custom
  (icomplete-matches-format nil)
  (icomplete-show-matches-on-no-input t))
(use-package icomplete
  :demand t
  :bind ( :map icomplete-minibuffer-map
          ("RET" . icomplete-force-complete-and-exit)
          ("C-j" . exit-minibuffer))
  :custom
  (icomplete-show-matches-on-no-input t)
  (icomplete-prospects-height 1)
  (icomplete-hide-common-prefix nil))
(use-package cua-base
  :hook (after-init . cua-mode)
  :custom
  (cua-delete-selection t)
  (cua-enable-cua-keys nil)
  (cua-prefix-override-inhibit-delay 0.7))
(when t
  (use-package-when helm (eq j:completion-ui 'helm)
    :ensure
    :hook (after-init . helm-mode)
    :bind (("M-T"       . helm-semantic-or-imenu)
           ("M-y"       . helm-show-kill-ring)
           ("M-x"       . helm-M-x)
           ([remap find-file] . helm-find-files)
           ([remap switch-to-buffer] . helm-mini)
           :map mode-specific-map
           ("g"         . helm-rg)))
  (use-package-when helm-mode (eq j:completion-ui 'helm)
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
           ;; ("SPC"       . helm-all-mark-rings)
           ;; ("/"         . helm-dabbrev)
           ;; ("C-z"       . helm-toggle-suspend-update)
           ;; ("a"         . helm-apropos)
           ;; ("b"         . helm-descbinds)
           ;; ("g"         . helm-google-suggest)
           ;; ("o"         . helm-top)
           ;; ("q"         . helm-regexp)
           ;; ("r"         . helm-register)
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
    (fset 'helm-display-mode-line #'ignore)
    (defun helm-hide-mode-line ()
      (with-current-buffer helm-buffer
        (setq-local mode-line-format nil))))
  (use-package-when helm-dash (eq j:completion-ui 'helm)
    :ensure
    :bind (:map help-map (("d" . helm-dash)))
    :custom
    (dash-docs-browser-func 'eww))
  (use-package-when helm-files (eq j:completion-ui 'helm)
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
  (use-package-when helm-rtags (eq j:completion-ui 'helm)
    :ensure
    :custom
    (rtags-display-result-backend 'helm))
  (use-package-when helm-swoop (eq j:completion-ui 'helm)
    :ensure
    :bind ( :map help-map
            ("C-SPC"   . helm-swoop)
            ("M-,"     . helm-swoop-back-to-last-point)
            :map isearch-mode-map
            ("M-i"     . helm-swoop-from-isearch)
            :map helm-swoop-map
            ("C-r"     . helm-previous-line)
            ("C-s"     . helm-next-line)
            :map helm-multi-swoop-map
            ("C-r"     . helm-previous-line)
            ("C-s"     . helm-next-line)))
  (use-package-when helm-xref (eq j:completion-ui 'helm)
    :ensure))
(when t
  (use-package icomplete
    :hook (after-init . fido-mode))
  (use-package icomplete-vertical
    :disabled
    :ensure
    :hook (after-init . icomplete-vertical-mode)
    :bind (:map icomplete-minibuffer-map
                ("<down>" . icomplete-forward-completions)
                ("C-n" . icomplete-forward-completions)
                ("<up>" . icomplete-backward-completions)
                ("C-p" . icomplete-backward-completions)
                ("C-v" . icomplete-vertical-toggle))))
(when t
  (use-package ido
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
      (flx-ido-mode 1))))
(when t
  (use-package ivy
    :ensure
    :diminish
    :hook (after-init . ivy-mode)
    :bind (:map ivy-minibuffer-map
                ("C-SPC" . ivy-toggle-mark)
                ("C-@"   . ivy-toggle-mark)
                ("TAB"   . ivy-partial)
                ("C-i"   . ivy-partial)
                ("M-'"   . ivy-avy)
                ("M-SPC" . ivy-restrict-to-matches)

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
    (ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                             (t . orderless-ivy-re-builder)))
    (ivy-use-selectable-prompt t)
    (ivy-use-virtual-buffers t)
    (ivy-virtual-abbreviate 'abbreviate)
    (ivy-wrap nil)
    (minibuffer-depth-indicate-mode t)
    :config
    (defun ivy-toggle-mark-real ()
      (if (ivy--marked-p)
          (ivy--unmark (ivy-state-current ivy-last))
        (ivy--mark (ivy-state-current ivy-last)))
      (ivy-next-line))
    (defun ivy-toggle-mark ()
      (interactive)
      (if (and (called-interactively-p 'interactive)
               (null current-prefix-arg)
               (not (eq last-command 'ivy-toggle-mark))
               (not (sit-for set-mark-dwim-timeout)))
          (let ((cmd (lookup-key (current-active-maps) (read-key-sequence ""))))
            (if (eq cmd this-command)
                (call-interactively set-mark-dwim-repeat-action)
              (ivy-toggle-mark-real)
              (call-interactively cmd)))
        (ivy-toggle-mark-real)))
    (setq search-default-mode #'char-fold-to-regexp)
    (advice-add 'ivy-switch-buffer :around
                (defun ivy-switch-buffer-maybe-other-window (o &rest args)
                  (if current-prefix-arg
                      (call-interactively 'ivy-switch-buffer-other-window)
                    (apply o args))))
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-arrow))
  (use-package ivy-hydra
    :ensure
    :after ivy)
  (use-package ivy-prescient
    :ensure
    :hook (after-init . ivy-prescient-mode))
  (use-package counsel
    :ensure
    :diminish
    :hook (after-init . counsel-mode)
    :bind (([remap dired] . counsel-dired)
           ([remap tmm-menubar] . counsel-tmm)
           ([remap menu-bar-open] . counsel-tmm)
           ([remap package-install] . counsel-package)
           ("M-T"   . counsel-semantic-or-imenu)
           ("M-y"   . counsel-yank-pop)
           ("M-\""  . counsel-register)

           :map ivy-mode-map
           ([remap switch-to-buffer] . counsel-switch-buffer)

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
           ("C-g" . counsel-grep)
           ("g"   . counsel-rg)
           ("o"   . counsel-grep-or-swiper)
           ("O"   . counsel-outline)

           :map comint-mode-map
           ("M-r" . counsel-shell-history))
    :custom
    (counsel-find-file-at-point t)
    (counsel-preselect-current-file t)
    (counsel-grep-base-command "rg -S -M 120 --no-heading --line-number --color never %s %s")
    (counsel-rg-base-command "rg -S -M 120 --no-heading --line-number --color never %s")
    (counsel-yank-pop-separator "\n----\n")
    :config
    (advice-add 'counsel-switch-buffer :around
                (defun counsel-switch-buffer-maybe-other-window (o &rest args)
                  (if current-prefix-arg
                      (call-interactively 'counsel-switch-buffer-other-window)
                    (apply o args))))
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
        ("M-/" find-file-recursively "search recursively")
        ("M-." open-dwim-from-find-file "open dwim")))
    (ivy-add-actions
     'counsel-find-file
     `(,@counsel-find-file-extra-actions))
    (dolist (c (mapcar #'car counsel-find-file-extra-actions))
      (define-key counsel-find-file-map
                  (kbd c) (ivy-make-magic-action 'counsel-find-file c)))
    (defun open-dwim-from-find-file (&optional _)
      (interactive)
      (setq unread-command-events
            (nconc (listify-key-sequence (kbd "M-9")) unread-command-events)))
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
    :ensure
    :bind (;; ([remap isearch-forward] . swiper-isearch)
           ;; ([remap isearch-backward] . swiper-isearch-backward)
           :map isearch-mode-map
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
    :ensure
    :hook (after-init . ivy-rich-mode))
  (use-package ivy-rtags
    :ensure
    :after rtags
    :custom
    (rtags-display-result-backend 'ivy))
  (use-package ivy-xref
    :ensure
    :after xref
    :custom
    (xref-show-xrefs-function 'ivy-xref-show-xrefs)
    (xref-show-definitions-function #'ivy-xref-show-defs)))
(when t
  (use-package selectrum
    :ensure
    :bind ( :map help-map ("M-q" . selectrum-cycle-display-style)
            :map mode-specific-map ("C-r" . selectrum-repeat))
    :hook (after-init . selectrum-mode)
    :custom
    (selectrum-complete-in-buffer nil)
    (selectrum-count-style nil)
    (selectrum-max-window-height .15)
    (file-name-shadow-properties '(invisible t))
    (orderless-skip-highlighting (lambda () selectrum-is-active))
    (selectrum-highlight-candidates-function #'orderless-highlight-matches)
    :config
    (use-package prescient
      :ensure
      :hook (after-init . prescient-persist-mode))
    (use-package selectrum-prescient
      :ensure
      :custom
      (selectrum-prescient-enable-filtering nil)
      :config
      (selectrum-prescient-mode 1))
    (use-package consult-dir
      :ensure t
      :bind ( :map minibuffer-local-map
              ("M-/" . consult-dir-jump-file))))
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
           ("e" . consult-error)
           :map search-map
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
                        (consult-imenu--select
                         "Go to item: "
                         (consult-imenu--multi-items buffers)))
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
     '(marginalia-annotators-light marginalia-annotators-heavy))
    :hook (after-init . marginalia-mode)
    :config
    (advice-add #'marginalia-cycle :after
                (lambda () (when (bound-and-true-p selectrum-mode)
                             (selectrum-exhibit 'keep-selected)))))
  (use-package embark
    :ensure
    :after selectrum
    :commands (embark-act embark-prefix-help-command)
    :bind (:map minibuffer-local-map
                ("M-."   . embark-act)
                ("M-,"   . embark-act-noquit)
                ("M-E"   . embark-export)
                :map selectrum-minibuffer-map
                ("M-."   . embark-act)
                ("M-,"   . embark-act-noquit)
                ("M-E"   . embark-export))

    :custom
    (prefix-help-command #'embark-prefix-help-command)
    (embark-cycle-key ";")
    :config
    (defun embark-act-noquit ()
      "Run action but don't quit the minibuffer afterwards."
      (interactive)
      (let ((embark-quit-after-action nil))
        (embark-act)))
    (advice-add 'kill-line :around #'consult-kill-line-dwim)
    (defun consult-kill-line-dwim (o &rest args)
      (let ((target (and (minibufferp) (car (embark--targets)))))
        (cond ((eq 'buffer (car target))
               (kill-buffer (cl-second target)))
              ((eq 'file (car target))
               (delete-file (cl-second target)))
              (t
               (apply o args))))))
  (use-package embark-consult
    :ensure
    :after (embark consult)
    :config
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)))
(use-package compile
  :diminish compilation-in-progress
  :hook ((compilation-mode . run-before-compile)
         (compilation-filter . apply-xterm-color-filter))
  :bind (("<f7>" . compile)
         ("<f8>" . recompile)
         :map compilation-shell-minor-mode-map
         ("g" . recompile)
         ([remap read-only-mode] . compilation-toggle-shell-mode)
         :map compilation-mode-map
         ([remap read-only-mode] . compilation-toggle-shell-mode))
  :custom
  (compilation-environment '("TERM=xterm-256color"))
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-save-buffers-predicate (lambda ()))
  (compilation-scroll-output 'first-error)
  :config
  (use-package xterm-color
    :ensure
    :functions xterm-color-filter)
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
                           (memq major-mode '(comint-mode compilation-mode))
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
  (defvar compile-terminal-integration-mode 'mine) ; mine|coterm|eat
  (pcase compile-terminal-integration-mode
    ('mine
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
    ('coterm
     (use-package coterm :ensure)
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
     (advice-add #'compilation-start :filter-args #'use-comint-always)
     (defun use-comint-always (args)
       (cl-list* (car args) t (cddr args))))
    ('eat
     (use-package eat :ensure)
     (advice-add #'compilation-start :around #'start-file-process-shell-command-with-eat)
     (defun start-file-process-shell-command-with-eat (o &rest args)
       (cl-letf (((symbol-function 'start-file-process-shell-command)
                  (lambda (name buffer command)
                    (require 'eat)
                    (with-current-buffer (eat-exec buffer name "bash" nil (list "-ilc" command))
                      (eat-emacs-mode)
                      (setq eat--synchronize-scroll-function #'eat--synchronize-scroll)
                      (get-buffer-process (current-buffer))))))
         (apply o args)))
     (add-hook #'compilation-start-hook #'enable-eat-on-compilation)
     (defun enable-eat-on-compilation (proc)
       (set-process-filter proc #'eat--filter)
       (add-function :after (process-sentinel proc) #'eat--sentinel))))
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
  (defun bpo-flush (proc)
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (when-let (timer (process-get proc :timer))
          (cancel-timer timer)
          (process-put proc :timer nil))
        (let ((inhibit-quit t)
              (inhibit-read-only t)
              (inhibit-modification-hooks t)
              (queue (process-get proc :queue)))
          (process-put proc :queue nil)
          ;; By the time delayed filter is called, process may be dead.
          (cl-letf (((symbol-function 'get-buffer-process)
                     (lambda (&rest _) proc)))
            (dolist (p (nreverse queue))
              (funcall (car p) proc (cdr p))))))))
  (defun bpo-enqueue (ofun proc o)
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (process-put proc :queue (cons (cons ofun o) (process-get proc :queue)))
        (unless (process-get proc :timer)
          (process-put proc :timer (run-with-timer 0.2 nil #'bpo-flush proc))))))
  (defun handle-process-buffered (proc)
    (add-function :around (process-filter proc) #'bpo-enqueue)
    (add-function :around (process-sentinel proc) #'bpo-enqueue))
  (add-hook 'compilation-start-hook #'handle-process-buffered)
  (add-hook 'compilation-filter-hook #'apply-xterm-color-filter)
  (use-package xterm-color :ensure :functions xterm-color-filter)
  (defun apply-xterm-color-filter ()
    (let* ((proc (get-buffer-process (current-buffer)))
           (end-marker (and proc (process-mark proc)))
           (inhibit-redisplay t))
      (save-excursion
        (goto-char compilation-filter-start)
        (while (re-search-forward (rx "\033[" (group (*? num)) (group (any "GADJK"))) end-marker t)
          (let ((count (and (> (length (match-string 1)) 0) (string-to-number (match-string 1))))
                (c (aref (match-string 2) 0)))
            (delete-region (match-beginning 0) (point))
            (cond ((= c ?G)
                   (delete-region (pos-bol) (point)))
                  ((= c ?A)
                   (delete-region (pos-bol (- (1- (or count 1)))) (point)))
                  ((= c ?D)
                   (delete-region (- (point) (or count 1)) (point)))
                  ((= c ?K)
                   ;; 0 (or missing) -> point to eol
                   ;; 1 -> bol to point
                   ;; 2 -> bol to eol
                   (unless (= 0 (or count 0))
                     (delete-region (pos-bol) (point))))
                  ((= c ?J)
                   ;; 0 (or missing) -> point to end of screen
                   ;; 1 -> beginning of screen to point
                   ;; 2 -> entire screen
                   ;; 3 -> entire screen & all lines in scollback buffer
                   ))
            (setq compilation-filter-start (min (point) compilation-filter-start))))
        (goto-char compilation-filter-start)
        (let* ((s (buffer-substring-no-properties compilation-filter-start end-marker))
               (ns (ansi-color-apply (xterm-color-filter s))))
          (if (string-equal s ns)
              (goto-char end-marker)
            (delete-region compilation-filter-start end-marker)
            (insert ns)))
        (set-marker end-marker (point)))))
  (defun do-kill-compilation (o &rest args)
    (when (and (called-interactively-p 'interactive)
               (memq major-mode '(comint-mode compilation-mode))
               (get-buffer-process (current-buffer)))
      (kill-compilation))
    (apply o args))
  (advice-add #'recompile :around #'do-kill-compilation)
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
(use-package eshell
  :hook (eshell-pre-command . eshell-show-time)
  :bind (("C-`"  . eshell)
         :map eshell-mode-map
         ([remap eshell-previous-matching-input] . consult-history))
  :custom
  (eshell-hist-ignoredups t)
  :config
  (use-package esh-autosuggest :ensure :hook (eshell-mode . esh-autosuggest-mode))
  (advice-add 'eshell-list-history :override 'consult-history)
  (defun eshell-show-time ()
    (eshell-interactive-print
     (let ((s (format-time-string "%m-%d %T\n")))
       (concat (propertize " " 'display `(space :align-to (- right-fringe ,(length s))))
               s)))))
(use-package tempel
  :ensure
  :hook ((prog-mode text-mode org-mode) . tempel-setup-capf)
  :config
  (use-package tempel-collection :ensure t)
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions (cons #'tempel-expand completion-at-point-functions))))
(use-package tree-sitter
  :disabled
  :unless (treesit-available-p)
  :ensure
  :hook ((tree-sitter-after-on . tree-sitter-hl-mode)
         ((rustic-mode c-mode-common) . tree-sitter-mode))
  :config
  (use-package tree-sitter-langs :ensure))
(when (file-directory-p "~/work/nano-emacs")
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

(when nil
  (csetq prefix-help-command 'dispatch-command-with-prefix)
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
  (defun completion-in-region-on-minibuffer (start end collection &optional predicate)
    (if (and (minibufferp) (not (string= (minibuffer-prompt) "Eval: ")))
        (completion--in-region start end collection predicate)
      (let* ((initial (buffer-substring-no-properties start end))
             (limit (car (completion-boundaries initial collection predicate "")))
             (all (completion-all-completions initial collection predicate (length initial)))
             (completion (cond
                          ((atom all) nil)
                          ((and (consp all) (atom (cdr all)))
                           (concat (substring initial 0 limit) (car all)))
                          (t (completing-read "Completion: " collection predicate t initial)))))
        (if (null completion)
            (progn (message "No completion") nil)
          (delete-region start end)
          (insert completion)
          t))))
  (setq completion-in-region-function #'completion-in-region-on-minibuffer))
