;; -*- lexical-binding: t -*-
(unless noninteractive
  (setq native-comp-speed 2
        comp-speed 2)
  (setq native-comp-async-report-warnings-errors nil
        comp-async-report-warnings-errors nil)
  (setq native-comp-async-query-on-exit t
        comp-async-query-on-exit t)
  (setq gc-cons-threshold most-positive-fixnum)
  (setq site-run-file nil)
  (menu-bar-mode -1)
  (line-number-mode -1)
  (tooltip-mode -1)
  (setq widget-image-enable nil)
  (setq frame-inhibit-implied-resize t)
  (setq load-prefer-newer t)
  (setq default-frame-alist '((menu-bar-lines . 0)
                              (tool-bar-lines . 0)
                              (vertical-scroll-bars . nil)
                              (alpha            . (100 95))
                              (left-fringe      . 0)
                              (right-fringe     . 0)
                              (ns-transparent-titlebar . t)
                              (undecorated . t)
                              (internal-border-width . 8)
                              (wait-for-wm      . nil)))
  (push '(font . "Cascadia Code-12") default-frame-alist)
  ;; (custom-set-faces '(mode-line          ((t :inverse-video t :style nil))))
  (set-face-attribute 'mode-line nil :box nil :overline nil :underline nil)
  (when nil
    (let ((fg-color "#dcdccc")
          (bg-color "#3f3f3f")
          (background-mode 'dark))
      (when (eq background-mode 'light)
        (setq fg-color bg-color
              bg-color fg-color))

      (setq default-frame-alist
            (nconc default-frame-alist
                   `((foreground-color . ,fg-color)
                     (background-color . ,bg-color))))
      (custom-set-variables
       `(frame-background-mode ',background-mode))
      (custom-set-faces
       `(mode-line          ((t :foreground ,bg-color :background ,fg-color :box nil)))
       '(mode-line-inactive ((t :inherit mode-line)))
       '(vertical-border    ((t :foreground "grey30" :background "grey30" :box nil)))
       '(header-line                 ((t :inherit mode-line)))
       '(line-number-current-line    ((t :inherit hl-line)))
       `(region                      ((t :background ,(if (eq background-mode 'dark)
                                                          "#6f6f6f"
                                                        "lightblue3"))))
       `(hl-line                     ((t :background ,(if (eq background-mode 'dark)
                                                          "#4f4f4f"
                                                        "lightblue2"))))
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
       '(ivy-current-match           ((t :inherit secondary-selection)))))))
