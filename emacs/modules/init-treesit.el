;;; init-treesit.el --- Treesit Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Treesit Configuration

;;; Code:


;;; TREESITTER-AUTO
;; Treesit-auto simplifies the use of Tree-sitter grammars in Emacs,
;; providing automatic installation and mode association for various
;; programming languages. This enhances syntax highlighting and
;; code parsing capabilities, making it easier to work with modern
;; programming languages.
(use-package treesit-auto
  :ensure t
  :straight t
  :after emacs
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))



(provide 'init-treesit)
;;; init-treesit.el ends here
