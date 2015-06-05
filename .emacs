(when (< emacs-major-version 27)
  (load "~/.emacs.d/early-init" nil t))

(load "~/.emacs.d/init" nil t)

(unless (and (require 'server nil t) (server-running-p))
  (server-start))
(or window-system
    (xterm-mouse-mode t))
