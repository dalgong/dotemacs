(defun disable-display-update (&optional arg)
  (setq inhibit-redisplay arg
        inhibit-message   arg))
(disable-display-update t)
(add-hook #'after-init-hook #'disable-display-update 100)

(setq elfeed-feeds
      '("https://xkcd.com/rss.xml"
        "https://nullprogram.com/feed/"
        "http://pragmaticemacs.com/feed/"
        "https://blog.codinghorror.com/rss/"
        "https://daringfireball.net/feeds/main"))

(let ((file-name-handler-alist nil))
  (load "~/.emacs.d/init" nil t))

(setq frame-title-format "%b")
