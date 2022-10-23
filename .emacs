(setq inhibit-redisplay t
      inhibit-message   t)
(add-hook #'after-init-hook
          (defun re-enable-display-update ()
            (setq inhibit-redisplay nil
                  inhibit-message   nil))
          100)
(setq elfeed-feeds
      '("https://xkcd.com/rss.xml"
        "https://nullprogram.com/feed/"
        "http://pragmaticemacs.com/feed/"
        "https://blog.codinghorror.com/rss/"
        "https://daringfireball.net/feeds/main"))

(let ((file-name-handler-alist nil))
  (load "~/.emacs.d/init" nil t))

(setq frame-title-format "%b")
