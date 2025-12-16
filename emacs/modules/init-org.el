;;; init-org.el --- ORG Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; ORG Configuration

;;; Code:


;;; ORG-MODE
;; Org-mode is a powerful system for organizing and managing your notes,
;; tasks, and documents in plain text. It offers features like task management,
;; outlining, scheduling, and much more, making it a versatile tool for
;; productivity. The configuration below simply defers loading Org-mode until
;; it's explicitly needed, which can help speed up Emacs startup time.
(use-package org
  :ensure nil     ;; This is built-in, no need to fetch it.
  :defer t
  :config
  (setq
   org-directory "~/org/"
   org-agenda-files  '("~/org/calendar.org")))


(provide 'init-org)
;;; init-org.el ends here
