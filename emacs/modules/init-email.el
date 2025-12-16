;;; init-email.el --- Email Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Email configuration

;;; Code:

;;; NOTMUCH
(require 'notmuch)

(setq notmuch-show-empty-saved-searches t)
(setq notmuch-saved-searches
	  '((:name "unread"
			   :query "tag:inbox and tag:unread"
			   :count-query "tag:inbox and tag:unread"
			   :sort-order newest-first)
		(:name "inbox"
			   :query "tag:inbox"
			   :count-query "tag:inbox"
			   :sort-order newest-first)
		(:name "gmail"
			   :query "folder:gmail/Inbox"
			   :sort-order newest-first)
		(:name "nablesolutions"
			   :query "folder:nablesolutions/Inbox"
			   :sort-order newest-first)
		(:name "support"
			   :query "folder:support/Inbox"
			   :sort-order newest-first)))

(defun my-notmuch-mua-empty-subject-check ()
  "Request confirmation before sending a message with empty subject"
  (when (and (null (message-field-value "Subject"))
			 (not (y-or-n-p "Subject is empty, send anyway? ")))
	(error "Sending message cancelled: empty subject.")))
(add-hook 'message-send-hook 'my-notmuch-mua-empty-subject-check)

(use-package notmuch-indicator
  :ensure t
  :defer t
  :init
  (setq notmuch-indicator-args
		'((:terms "tag:inbox and tag:unread" :label "󰇮 ")))
  :config
  (notmuch-indicator-mode 1))


(provide 'init-email)
;;; init-email.el ends here
