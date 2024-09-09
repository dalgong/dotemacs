(setq elfeed-feeds
      '("https://xkcd.com/rss.xml"
        "https://nullprogram.com/feed/"
        "http://pragmaticemacs.com/feed/"
        "https://blog.codinghorror.com/rss/"
        "https://daringfireball.net/feeds/main"
        "https://feeds.feedburner.com/explainextended"
        "http://morss.aryadevchavali.com/news.ycombinator.com/rss"
        "http://morss.aryadevchavali.com/feeds.bbci.co.uk/news/rss.xml"
        "http://morss.aryadevchavali.com/feeds.bbci.co.uk/news/technology/rss.xml"))

(let ((file-name-handler-alist nil)
      (inhibit-message t))
  (load "~/.emacs.d/init" nil t)
  (when (display-graphic-p)
    ;; use setq if you want
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
    (load-theme 'modus-operandi t)))

