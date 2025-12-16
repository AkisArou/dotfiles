;;; init-format.el --- Formatting Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Formatting Configuration

;;; Code:

;; APHELEIA
(use-package apheleia
  :ensure t
  :config
  ;; Define prettierd formatter
  (setf (alist-get 'prettierd apheleia-formatters)
		'("prettierd" "--stdin-filepath" filepath))

  ;; Associate modes
  (dolist (mode '(typescript-mode
				  typescript-ts-mode
				  javascript-mode
				  javascript-ts-mode
				  tsx-ts-mode
				  yaml-ts-mode
				  json-mode
				  css-mode
				  html-mode))
	(setf (alist-get mode apheleia-mode-alist) 'prettierd))

  (apheleia-global-mode +1))


(provide 'init-format)
;;; init-format.el ends here
