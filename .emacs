(setq comp-async-report-warnings-errors nil)
(when (< emacs-major-version 27)
  (load "~/.emacs.d/early-init" nil t))

(setq elfeed-feeds
      '("https://xkcd.com/rss.xml"
        "https://nullprogram.com/feed/"
        "http://pragmaticemacs.com/feed/"
        "https://blog.codinghorror.com/rss/"
        "https://daringfireball.net/feeds/main"))

(load "~/.emacs.d/init" nil t)

(unless (and (require 'server nil t) (server-running-p))
  (server-start))
(or window-system
    (xterm-mouse-mode t))
(when (require 'zerodark-theme nil t)
  (let ((class '((class color) (min-colors 89))))
    (custom-theme-set-faces
     'zerodark
     `(selectrum-current-candidate
       ((,class (:background "#48384c" :weight bold :foreground "#c678dd"))))
     `(selectrum-prescient-primary-highlight
       ((,class (:foreground "#da8548"))))
     `(selectrum-prescient-secondary-highlight
       ((,class (:foreground "#98be65"))))))
  (enable-theme 'zerodark))
