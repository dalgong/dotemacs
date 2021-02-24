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
    (use-package flx :ensure)
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
    :ensure
    :after ivy)
  (use-package counsel
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
