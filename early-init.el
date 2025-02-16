;; -*- lexical-binding: t -*-
(unless noninteractive
  (setq comp-async-query-on-exit t
        comp-async-report-warnings-errors nil
        comp-speed 2
        native-comp-async-query-on-exit t
        native-comp-async-report-warnings-errors nil
        native-comp-speed 2)
  (setq gc-cons-threshold most-positive-fixnum)
  (setq site-run-file nil)
  (menu-bar-mode -1)
  (line-number-mode -1)
  (tooltip-mode -1)
  (setq widget-image-enable nil)
  (setq frame-inhibit-implied-resize t)
  (setq load-prefer-newer t)
  (setq default-frame-alist
	`((menu-bar-lines . 0)
          (tool-bar-lines . 0)
          (vertical-scroll-bars . nil)
          (alpha            . (100 95))
          (left-fringe      . 0)
          (right-fringe     . 0)
          (ns-transparent-titlebar . t)
          ;; (undecorated . t)
          (internal-border-width . 8)
          (wait-for-wm      . nil)
          (font . ,(format "GoMono Nerd Font-%d" (if (eq system-type 'gnu/linux) 11 12)))))
  (let ((original-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil
	  inhibit-message t)
    (add-to-list
     'after-init-hook
     (lambda ()
       (setq file-name-handler-alist original-file-name-handler-alist)
       (setopt
	modus-themes-italic-constructs        t
	modus-themes-bold-constructs          t
	modus-themes-mixed-fonts              t
	modus-themes-common-palette-overrides '((border-mode-line-active bg-mode-line-active)
						(border-mode-line-inactive bg-mode-line-inactive)
						(comment yellow)
						(string green-warmer))
	modus-vivendi-palette-overrides       '((bg-main "#1d2021")
						(fg-main "#c2c2c2")))
       (load-theme 'modus-operandi t)))))
