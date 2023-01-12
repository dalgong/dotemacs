(defun disable-display-update (&optional arg)
  (setq inhibit-message   arg))
(disable-display-update t)
(add-hook #'after-init-hook #'disable-display-update 100)

(setq elfeed-feeds
      '("https://xkcd.com/rss.xml"
        "https://nullprogram.com/feed/"
        "http://pragmaticemacs.com/feed/"
        "https://blog.codinghorror.com/rss/"
        "https://daringfireball.net/feeds/main"))

(when (eq 'ns window-system)
  (setenv "LIBRARY_PATH"
          "/Applications/Emacs.app/Contents/Frameworks/gcc/12:/Applications/Emacs.app/Contents/Frameworks/gcc/12/gcc/x86_64-apple-darwin20/12:/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"))
(let ((file-name-handler-alist nil))
  (load "~/.emacs.d/init" nil t))

(setq frame-title-format "%b")
;; minimalistic
(custom-set-faces
 '(mode-line          ((t :inverse-video nil :bold t :style nil)))
 '(mode-line-inactive ((t :inverse-video nil :style nil))))
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?┃))
(setq-default mode-line-format
              '("%e %* %b "
                (:propertize (" ")
                             display (space :align-to (- right 11)))
                "%l,%c  %p"))
