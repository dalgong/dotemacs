(setq elfeed-feeds
      '("https://xkcd.com/rss.xml"
        "https://nullprogram.com/feed/"
        "http://pragmaticemacs.com/feed/"
        "https://blog.codinghorror.com/rss/"
        "https://daringfireball.net/feeds/main"))

(let ((file-name-handler-alist nil))
  (load "~/.emacs.d/init" nil t))
