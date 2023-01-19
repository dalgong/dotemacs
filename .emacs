(defun disable-display-update (&optional arg)
  (setq inhibit-message arg)
  (or arg (use-package powerline :ensure t :config (powerline-default-theme))))
(disable-display-update t)
(add-hook #'after-init-hook #'disable-display-update 100)

(setq elfeed-feeds
      '("https://xkcd.com/rss.xml"
        "https://nullprogram.com/feed/"
        "http://pragmaticemacs.com/feed/"
        "https://blog.codinghorror.com/rss/"
        "https://daringfireball.net/feeds/main"
        "https://www.theonion.com/rss"
        "http://morss.aryadevchavali.com/news.ycombinator.com/rss"
        "http://morss.aryadevchavali.com/feeds.bbci.co.uk/news/rss.xml"
        "http://morss.aryadevchavali.com/feeds.bbci.co.uk/news/technology/rss.xml"))

(when (eq 'ns window-system)
  (setenv "LIBRARY_PATH"
          "/Applications/Emacs.app/Contents/Frameworks/gcc/12:/Applications/Emacs.app/Contents/Frameworks/gcc/12/gcc/x86_64-apple-darwin20/12:/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"))
(let ((file-name-handler-alist nil))
  (load "~/.emacs.d/init" nil t))

(setq frame-title-format "%b")
(when nil
  ;; minimalistic
  (custom-set-faces
   '(mode-line          ((t :inverse-video nil :bold t :style nil)))
   '(mode-line-inactive ((t :inverse-video nil :style nil))))
  (defvar original-mode-line-format mode-line-format)
  (advice-add #'what-cursor-position :after
              (lambda (_)
                (let ((cv mode-line-format))
                  (unwind-protect
                      (progn
                        (setq-local mode-line-format original-mode-line-format)
                        (force-mode-line-update)
                        (sit-for 3))
                    (setq-local mode-line-format cv)
                    (force-mode-line-update)))))
  (setq-default mode-line-format
                '("%e %* %b "
                  (:propertize (" ")
                               display (space :align-to (- right 11)))
                  "%l,%c  %p")))
