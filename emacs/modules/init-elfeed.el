;;; init-elfeed.el --- Elfeed Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Elfeed configuration

;;; Code:

(use-package elfeed
  :ensure t
  :config
  (setq-default elfeed-search-filter "")

  (setq elfeed-feeds
        '("https://expo.dev/changelog/rss.xml"
          "https://reactnative.dev/blog/rss.xml"
          "https://devblogs.microsoft.com/typescript/feed/"))
  )


(provide 'init-elfeed)
;;; init-elfeed.el ends here
