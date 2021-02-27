;; -*- lexical-binding: t -*-
(unless noninteractive
  (setq default-frame-alist `(;; (undecorated . t)
                              (background-mode  . dark)
                              (background-color . "#3f3f3f")
                              (foreground-color . "#dcdccc")
                              (menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (vertical-scroll-bars . nil)
                              (alpha            . (100 95))
                              (font             . "IBM Plex Mono-13:antialias=true:hinting=true")
                              (left-fringe      . 0)
                              (right-fringe     . 0)
                              (ns-transparent-titlebar . t)
                              (internal-border-width . 8)
                              (wait-for-wm      . nil)))
  (setq site-run-file nil)
  (let* ((bindings `((inhibit-redisplay . nil)
                     (gc-cons-threshold . ,(* 800 1024 1024))
                     (gc-cons-percentage . 0.6)
                     (file-name-handler-alist . nil)))
         (old-values (mapcar (lambda (p) (symbol-value (car p))) bindings)))
    (dolist (b bindings) (set (car b) (cdr b)))
    (add-hook 'emacs-startup-hook
              (defun restore-values ()
                (while bindings
                  (set (car (pop bindings)) (pop old-values))))))
  (menu-bar-mode -1)
  (line-number-mode -1)
  (tooltip-mode -1)
  (setq widget-image-enable nil)
  (setq load-prefer-newer t)
  (when nil
    (custom-set-variables
     `(frame-background-mode 'dark))
    (custom-set-faces
     '(mode-line          ((t :foreground "#3f3f3f" :background "#dcdccc" :box nil)))
     '(mode-line-inactive ((t :foreground "grey20" :background "grey30" :box nil)))
     '(vertical-border    ((t :foreground "grey30" :background "grey30" :box nil)))
     '(header-line                 ((t :inherit mode-line)))
     '(line-number-current-line    ((t :inherit hl-line)))
     '(region                      ((t :background "#6f6f6f")))
     '(hl-line                     ((t :background "#4f4f4f")))
     '(diff-added                  ((t :foreground "green"  :background "gray10")))
     '(diff-removed                ((t :foreground "yellow" :background "gray10")))
     '(ediff-current-diff-A        ((t :background "gray20" :inherit diff-added)))
     '(ediff-current-diff-B        ((t :background "gray20" :inherit diff-added)))
     '(ediff-current-diff-C        ((t :background "gray20" :inherit diff-added)))
     '(ediff-current-diff-Ancestor ((t :background "gray20" :inherit diff-added)))
     '(ediff-fine-diff-A           ((t :background "gray30" :inherit diff-added)))
     '(ediff-fine-diff-B           ((t :background "gray30" :inherit diff-added)))
     '(ediff-fine-diff-C           ((t :background "gray30" :inherit diff-added)))
     '(ediff-fine-diff-Ancestor    ((t :background "gray30" :inherit diff-added)))
     '(ediff-even-diff-A           ((t :background "gray10")))
     '(ediff-even-diff-B           ((t :background "gray10")))
     '(ediff-even-diff-C           ((t :background "gray10")))
     '(ediff-even-diff-Ancestor    ((t :background "gray10")))
     '(ediff-odd-diff-A            ((t :background "gray15")))
     '(ediff-odd-diff-B            ((t :background "gray15")))
     '(ediff-odd-diff-C            ((t :background "gray15")))
     '(ediff-odd-diff-Ancestor     ((t :background "gray15")))
     '(diff-refine-added           ((t :background "gray30" :bold t :inherit diff-added)))
     '(diff-refine-change          ((t :background "gray30" :bold t :inherit diff-changed)))
     '(diff-refine-removed         ((t :background "gray30" :bold t :inherit diff-removed)))
     '(ivy-current-match           ((t :inherit secondary-selection)))
     '(helm-ff-prefix                   ((t :inherit dired-header)))
     '(helm-ff-executable               ((t :inherit dired-perm-write)))
     '(helm-ff-suid                     ((t :inherit dired-set-id)))
     '(helm-ff-directory                ((t :inherit dired-directory)))
     '(helm-ff-dotted-directory         ((t :inherit helm-ff-directory)))
     '(helm-ff-dotted-symlink-directory ((t :inherit helm-ff-symlink)))
     '(helm-ff-symlink                  ((t :inherit dired-symlink)))
     '(helm-ff-invalid-symlink          ((t :inherit error)))
     '(helm-ff-denied                   ((t :inherit error)))
     '(helm-ff-file                     ((t :inherit default)))
     '(helm-ff-truename                 ((t :inherit dired-flagged)))
     '(helm-ff-dirs                     ((t :inherit dired-directory)))
     '(helm-ff-socket                   ((t :inherit dired-special)))
     '(helm-ff-pipe                     ((t :inherit dired-special)))
     '(helm-ff-file-extension           ((t :inherit font-lock-type-face)))
     '(helm-ff-backup-file              ((t :inherit dired-ignored))))))
