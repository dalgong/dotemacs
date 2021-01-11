;; -*- lexical-binding: t -*-
(defvar use-dark-mode nil)
(defvar completion-framework 'icomplete)      ; '(ivy helm ido icomplete)
(defvar shell-variant 'shell)           ; '(eshell shell)

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
      (setq gc-cons-percentage 0.3)))
  (setq file-name-handler-alist nil)
  (advice-add 'require :around (lambda (o &rest args)
                                 (let ((file-name-handler-alist))
                                   (apply o args))))
  (menu-bar-mode -1)
  (line-number-mode -1)
  (tooltip-mode -1)
  (setq widget-image-enable nil)
  (setq load-prefer-newer t)
  (setq default-frame-alist `(
                              ;; ,@(and use-dark-mode
                              ;;        '((background-mode  . dark)
                              ;;          (background-color . "#3f3f3f")
                              ;;          (foreground-color . "#dcdccc")))
                              ;; (undecorated . t)
                              (tool-bar-lines . 0)
                              (vertical-scroll-bars . nil)
                              (alpha            . (100 95))
                              ;; (font             . "Cousine-14:antialias=true:hinting=true")
                              ;; (font             . "Anonymous Pro-16:antialias=true:hinting=true")
                              ;; (font             . "Go Mono-14:antialias=true:hinting=true")
                              (font             . "Fira Mono-14:antialias=true:hinting=true")
                              ;; (cursor-color     . "red")
                              ;; (border-color     . "grey30")
                              ;; (mouse-color      . "Grey")
                              (left-fringe      . 0)
                              (right-fringe     . 0)
                              (ns-transparent-titlebar . t)
                              (internal-border-width . 8)
                              (wait-for-wm      . nil)))
  (unless (and (eq system-type 'darwin) window-system)
    (push '(menu-bar-lines . 0) default-frame-alist)))
