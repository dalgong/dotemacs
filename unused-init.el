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
