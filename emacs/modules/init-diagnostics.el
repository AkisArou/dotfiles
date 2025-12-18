;;; init-diagnostics.el --- Diagnostics Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Diagnostics configuration

;;; Code:

;;; FLYCHECK
(use-package flycheck
  :ensure t
  :config
  (setq flycheck-display-errors-delay 0.25)
  :init (global-flycheck-mode))


(use-package flycheck-color-mode-line
  :ensure t
  :after flycheck
  :init (flycheck-color-mode-line-mode))

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

  (require 'flycheck-posframe)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(provide 'init-diagnostics)
;;; init-diagnostics.el ends here
