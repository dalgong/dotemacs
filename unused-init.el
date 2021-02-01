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
