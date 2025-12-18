;;; init-lang.el --- Programming Languages Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Programming Languages Configuration

;;; Code:

;;; MARKDOWN-MODE
;; Markdown Mode provides support for editing Markdown files in Emacs,
;; enabling features like syntax highlighting, previews, and more.
;; It’s particularly useful for README files, as it can be set
;; to use GitHub Flavored Markdown for enhanced compatibility.
(use-package markdown-mode
  :defer t
  :straight t
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)            ;; Use gfm-mode for README.md files.
  :init (setq markdown-command "multimarkdown")) ;; Set the Markdown processing command.

;;; DOTENV
;; A simple major mode to provide .env files with color highlighting
(use-package dotenv-mode
  :defer t
  :straight t
  :ensure t)


;;; JSON-MODE
(use-package json-mode
  :ensure t)

;;; YAML
(use-package yaml
  :ensure t)

;;; EMMET-MODE
(use-package emmet-mode
  :ensure t
  :hook ((html-mode . emmet-mode)
         (tsx-ts-mode . emmet-mode))
  :config
  (add-to-list 'emmet-jsx-major-modes 'tsx-ts-mode)
  (define-key emmet-mode-keymap (kbd "C-e") 'emmet-expand-line))


;;; ADD-NODE-MODULES-PATH
;; The `add-node-modules-path' package ensures that Emacs uses the local
;; `node_modules/.bin' for a project rather than globally installed binaries.
;; This is essential in JavaScript/TypeScript projects where different versions
;; of tools like `eslint' and `typescript-language-server' might be needed
;; per project.
;;
;; This setup helps prevent conflicts between global and local versions of
;; Node.js tools and ensures consistency across different environments.
;;
;; Example in the wild: This is an example of a real-world issue often faced
;; by developers using modern tech stacks. When working on multiple projects
;; with different dependencies, Emacs must use the correct local versions
;; instead of relying on globally installed packages. This configuration
;; ensures that the environment is accurate and project-specific tools are
;; properly utilized.
(use-package add-node-modules-path
  :ensure t
  :straight t
  :defer t
  :custom
  ;; Makes sure you are using the local bin for your
  ;; node project. Local eslint, typescript server...
  (eval-after-load 'typescript-ts-mode
    '(add-hook 'typescript-ts-mode-hook #'add-node-modules-path))
  (eval-after-load 'tsx-ts-mode
    '(add-hook 'tsx-ts-mode-hook #'add-node-modules-path))
  (eval-after-load 'typescriptreact-mode
    '(add-hook 'typescriptreact-mode-hook #'add-node-modules-path))
  (eval-after-load 'js-mode
    '(add-hook 'js-mode-hook #'add-node-modules-path)))


(provide 'init-lang)
;;; init-lang.el ends here
