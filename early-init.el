;; -*- lexical-binding: t -*-
(unless noninteractive
  (setq gc-cons-threshold most-positive-fixnum)
  (setq default-frame-alist
        `((menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (vertical-scroll-bars . nil)
          (alpha            . (100 95))
          (left-fringe      . 0)
          (right-fringe     . 0)
          ;; (undecorated . t)
          (internal-border-width . 8)
          (wait-for-wm      . nil)
          (font . ,(format "Jetbrains Mono-%d" (if (eq system-type 'gnu/linux) 11 12)))))
  ;; (setq initial-frame-alist '((visibility . nil)))
  (add-hook 'window-setup-hook
            (let ((original-file-name-handler-alist file-name-handler-alist))
              (setq file-name-handler-alist nil
                    inhibit-message t)
              (lambda ()
                (setq file-name-handler-alist original-file-name-handler-alist
                      inhibit-message nil)
                (load-theme 'doom-one t)))))
