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
  (load-theme 'modus-operandi t)
  (use-package bespoke-modeline
    :disabled
    :vc ( :url "https://github.com/mclear-tools/bespoke-modeline.git"
          :rev :newest)
    :hook after-init
    :init
    (setq bespoke-modeline-cleaner t)
    (setq bespoke-modeline-position 'bottom)
    (setq bespoke-modeline-space-top +0.1)
    (setq bespoke-modeline-space-bottom -0.1)
    (setq bespoke-modeline-visual-bell t)
    (setq bespoke-modeline-vc-symbol " G:")
    :config
    (load-theme 'modus-operandi t)))
