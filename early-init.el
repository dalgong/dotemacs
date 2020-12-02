;; -*- lexical-binding: t -*-
(unless noninteractive
  (setq site-run-file nil)
  (setq gc-cons-threshold (* 800 1024 1024))
  (setq gc-cons-percentage 0.6)
  (add-hook 'emacs-startup-hook 'revert-settings-for-startup)
  ;; (setq inhibit-redisplay t)
  (let ((backup-file-name-handler-alist file-name-handler-alist))
    (defun revert-settings-for-startup ()
      (setq file-name-handler-alist backup-file-name-handler-alist)
      (electric-pair-mode 1)
      (when (fboundp 'global-so-long-mode)
        (global-so-long-mode 1))
      (garbage-collect)
      (setq gc-cons-percentage 0.3)
      ;; (setq inhibit-redisplay nil)
      ;; (redisplay t)
      ))
  (setq file-name-handler-alist nil)
  (line-number-mode -1)
  (tooltip-mode -1)
  (defvar use-dark-mode t)
  (setq widget-image-enable nil)
  (setq default-frame-alist `(
                              ,@(and use-dark-mode
                                     '((background-mode  . dark)
                                       (background-color . "#3f3f3f")
                                       (foreground-color . "#dcdccc")))
                              ;; (undecorated . t)
                              (tool-bar-lines . 0)
                              (vertical-scroll-bars . nil)))
  (unless (and (eq system-type 'darwin) window-system)
    (push '(menu-bar-lines . 0) default-frame-alist))
  (setq window-system-default-frame-alist
        (let ((window-frame-parameters `((alpha            . (100 95))
                                         ;; (font             . "Cousine-14:antialias=true:hinting=true")
                                         ;; (font             . "Anonymous Pro-16:antialias=true:hinting=true")
                                         (font             . "IBM Plex Mono-14:antialias=true:hinting=true")
                                         ;; (font             . "Go Mono-14:antialias=true:hinting=true")
                                         ;; (cursor-color     . "red")
                                         ;; (border-color     . "grey30")
                                         (left-fringe      . 0)
                                         (right-fringe     . 9)
                                         ;; (mouse-color      . "Grey")
                                         (ns-transparent-titlebar . t)
                                         (internal-border-width . 8)
                                         (wait-for-wm      . nil))))
          `((x   ,@window-frame-parameters)
            (mac ,@window-frame-parameters)
            (ns  ,@window-frame-parameters)
            (nil ,@default-frame-alist)))))
