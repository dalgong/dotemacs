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
(use-package helm
  :disabled
  :ensure
  :hook (after-init . helm-mode)
  :bind (("M-T"       . helm-semantic-or-imenu)
         ("M-y"       . helm-show-kill-ring)
         ("M-x"       . helm-M-x)
         ([remap find-file] . helm-find-files)
         ([remap switch-to-buffer] . helm-mini)
         :map mode-specific-map
         ("g"         . helm-rg)))
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
(use-package helm-dash
  :disabled
  :ensure
  :bind (:map help-map (("d" . helm-dash)))
  :custom
  (dash-docs-browser-func 'eww))
(use-package helm-files
  :disabled
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
(use-package helm-rtags
  :disabled
  :custom
  (rtags-display-result-backend 'helm))
(use-package helm-swoop
  :disabled
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
(use-package helm-xref
  :disabled
  :ensure)
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

(when (and nil (file-directory-p "~/work/nano-emacs"))
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
