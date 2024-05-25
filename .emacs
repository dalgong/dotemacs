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
    (load-theme 'modus-operandi t)
    (dolist (ml '(mode-line mode-line-inactive))
      (set-face-attribute ml nil :box nil :overline nil :underline nil))))

