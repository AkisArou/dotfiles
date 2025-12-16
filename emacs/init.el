;;; init.el --- AkisArou config -*- lexical-binding: t; -*-

;;; Commentary:
;; Config

;;; Code:

;; Performance
;; Temporarily raise GC threshold during startup for speed
(defvar ek--startup-gc-cons-threshold #x40000000)
(setq gc-cons-threshold ek--startup-gc-cons-threshold)

;; Temporarily disable expensive file handlers during startup
(defvar ek--saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Set the maximum output size for reading process output, allowing for larger data transfers.
(setq read-process-output-max (* 1024 1024 4))

(setq package-enable-at-startup nil) ;; Disables the default package manager.
(setq user-emacs-directory (expand-file-name "emacs" (getenv "XDG_CONFIG_HOME")))

;; Bootstraps `straight.el'
(setq straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
	   (expand-file-name
		"straight/repos/straight.el/bootstrap.el"
		(or (bound-and-true-p straight-base-dir)
			user-emacs-directory)))
	  (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
	(with-current-buffer
		(url-retrieve-synchronously
		 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
		 'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package '(project :type built-in))
(straight-use-package 'use-package)


(require 'package)
(require 'project)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(use-package init-core)
(use-package init-ui)
(use-package init-editor)
(use-package init-dired)
(use-package init-treesit)
(use-package init-lang)
(use-package init-lsp)
(use-package init-completion)
(use-package init-diagnostics)
(use-package init-format)
(use-package init-vc)
(use-package init-org)
(use-package init-email)
(use-package init-dap)
(use-package init-evil)


(defun ek/first-install ()
  "Install tree-sitter grammars and compile packages on first run..."
  (interactive)
  (switch-to-buffer "*Messages*")
  (message ">>> All required packages installed.")
  (message ">>> Configuring Tree Sitter parsers...")

  (require 'treesit-auto)

  (setq treesit-auto-langs
		'(bash c css dockerfile html javascript jsdoc
			   lua markdown python toml
			   tsx typescript yaml))

  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1)
  (treesit-auto-install-all)

  (message ">>> Configuring Nerd Fonts...")
  (require 'nerd-icons)
  (nerd-icons-install-fonts)

  (message ">>> Press any key to close the installer and open Emacs normally. First boot will compile some extra stuff :)")
  (read-key)
  (kill-emacs))

(provide 'init)
;;; init.el ends here
