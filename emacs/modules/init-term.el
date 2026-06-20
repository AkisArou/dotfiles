;;; init-term.el --- UI Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Term Configuration

;;; Code:

(use-package vterm
  :ensure t
  :defer t
  :config
  ;; Once vterm is dead, the vterm buffer is useless. Why keep it around? We can
  ;; spawn another if want one.
  (setq vterm-kill-buffer-on-exit t)

  ;; 5000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 5000))

(provide 'init-term)
;;; init-term.el ends here
