(defun disable-display-update (&optional arg)
  (setq inhibit-message arg)
  (unless arg
    (load-theme 'modus-operandi t nil)
    (use-package powerline :ensure t :config (powerline-default-theme))))
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

(let ((file-name-handler-alist nil))
  (load "~/.emacs.d/init" nil t))
