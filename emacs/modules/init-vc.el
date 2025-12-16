;;; init-vc.el --- VC Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; VC Configuration

;;; Code:

;;; VC
;; The VC (Version Control) package is included here for awareness and completeness.
;; While its support for Git is limited and generally considered subpar, it is good to know
;; that it exists and can be used for other version control systems like Mercurial,
;; Subversion, and Bazaar.
;; Magit, which is often regarded as the "father" of Neogit, will be configured later
;; for an enhanced Git experience.
;; The keybindings below serve as a reminder of some common VC commands.
;; But don't worry, you can always use `M-x command' :)
(use-package vc
  :ensure nil                        ;; This is built-in, no need to fetch it.
  :defer t
  :bind
  (("C-x v d" . vc-dir)              ;; Open VC directory for version control status.
   ("C-x v =" . vc-diff)             ;; Show differences for the current file.
   ("C-x v D" . vc-root-diff)        ;; Show differences for the entire repository.
   ("C-x v v" . vc-next-action))     ;; Perform the next version control action.
  :config
  ;; Better colors for <leader> g b  (blame file)
  (setq vc-annotate-color-map
		'((20 . "#f5e0dc")
		  (40 . "#f2cdcd")
		  (60 . "#f5c2e7")
		  (80 . "#cba6f7")
		  (100 . "#f38ba8")
		  (120 . "#eba0ac")
		  (140 . "#fab387")
		  (160 . "#f9e2af")
		  (180 . "#a6e3a1")
		  (200 . "#94e2d5")
		  (220 . "#89dceb")
		  (240 . "#74c7ec")
		  (260 . "#89b4fa")
		  (280 . "#b4befe"))))

;;; FORGE
(use-package forge
  :after magit
  :ensure t
  :init
  (setq forge-add-default-sections t)
  (setq forge-add-default-bindings t))

;;; GIT-MODES
(use-package git-modes
  :ensure t
  :defer t)


;;; SMERGE
;; Smerge is included for resolving merge conflicts in files. It provides a simple interface
;; to help you keep changes from either the upper or lower version during a merge.
;; This package is built-in, so there's no need to fetch it separately.
;; The keybindings below did not needed to be setted, are here just to show
;; you how to work with it in case you are curious about it.
(use-package smerge-mode
  :ensure nil                                  ;; This is built-in, no need to fetch it.
  :defer t
  :bind (:map smerge-mode-map
			  ("C-c ^ u" . smerge-keep-upper)  ;; Keep the changes from the upper version.
			  ("C-c ^ l" . smerge-keep-lower)  ;; Keep the changes from the lower version.
			  ("C-c ^ n" . smerge-next)        ;; Move to the next conflict.
			  ("C-c ^ p" . smerge-previous)))  ;; Move to the previous conflict.



;;; GIT-TIMEMACHINE
(use-package git-timemachine
  :ensure t
  :straight (git-timemachine :type git :host github :repo "emacsmirror/git-timemachine")
  :defer t
  :init
  ;; Ensure the mode map exists before we define keys
  (with-eval-after-load 'git-timemachine
	(evil-define-key 'normal git-timemachine-mode-map
	  (kbd "C-j") 'git-timemachine-show-previous-revision
	  (kbd "C-k") 'git-timemachine-show-next-revision
	  (kbd "gb")  'git-timemachine-blame
	  (kbd "gtc") 'git-timemachine-show-commit))
  :config
  (setq git-timemachine-show-minibuffer-details t))


;;; DIFF-HL
;; The `diff-hl' package provides visual indicators for version control changes
;; directly in the margin of the buffer, showing lines added, deleted, or changed.
;; This is useful for tracking modifications while you edit files. When enabled,
;; it automatically activates in every buffer that has a corresponding version
;; control backend, offering a seamless experience.
;;
;; In comparison, Neovim users often rely on plugins like `gitsigns.nvim' or
;; `vim-signify', which provide similar functionalities by displaying Git
;; changes in the gutter and offer additional features like highlighting
;; changed lines and displaying blame information. `diff-hl' aims to provide
;; a comparable experience in Emacs with its own set of customizations.
(use-package diff-hl
  :defer t
  :straight t
  :ensure t
  :hook
  (find-file . (lambda ()
				 (global-diff-hl-mode)           ;; Enable Diff-HL mode for all files.
				 (diff-hl-flydiff-mode)          ;; Automatically refresh diffs.
				 (diff-hl-margin-mode)))         ;; Show diff indicators in the margin.
  :custom
  (diff-hl-side 'left)                           ;; Set the side for diff indicators.
  (diff-hl-margin-symbols-alist '((insert . "┃") ;; Customize symbols for each change type.
								  (delete . "-")
								  (change . "┃")
								  (unknown . "┆")
								  (ignored . "i"))))


;;; MAGIT
;; `magit' is a powerful Git interface for Emacs that provides a complete
;; set of features to manage Git repositories. With its intuitive interface,
;; you can easily stage, commit, branch, merge, and perform other Git
;; operations directly from Emacs. Magit’s powerful UI allows for a seamless
;; workflow, enabling you to visualize your repository's history and manage
;; changes efficiently.
;;
;; In the Neovim ecosystem, similar functionality is provided by plugins such as
;; `fugitive.vim', which offers a robust Git integration with commands that
;; allow you to perform Git operations directly within Neovim. Another popular
;; option is `neogit', which provides a more modern and user-friendly interface
;; for Git commands in Neovim, leveraging features like diff views and staging
;; changes in a visual format. Both of these plugins aim to replicate and
;; extend the powerful capabilities that Magit offers in Emacs.
(use-package magit
  :ensure t
  :straight t
  :defer t
  :config
  (if ek-use-nerd-fonts   ;; Check if nerd fonts are being used
	  (setopt magit-format-file-function #'magit-format-file-nerd-icons)) ;; Turns on magit nerd-icons
  )


;;; CONSULT-GH
(use-package consult-gh
  :after consult
  :ensure t
  :custom
  (consult-gh-default-clone-directory "~/projects")
  (consult-gh-show-preview t)
  (consult-gh-preview-key "C-o")
  (consult-gh-repo-action #'consult-gh--repo-browse-files-action)
  (consult-gh-large-file-warning-threshold 2500000)
  (consult-gh-confirm-name-before-fork nil)
  (consult-gh-confirm-before-clone t)
  (consult-gh-notifications-show-unread-only nil)
  (consult-gh-default-interactive-command #'consult-gh-transient)
  (consult-gh-prioritize-local-folder nil)
  (consult-gh-group-dashboard-by :reason)
	;;;; Optional
  (consult-gh-repo-preview-major-mode nil) ; show readmes in their original format
  (consult-gh-preview-major-mode 'org-mode) ; use 'org-mode for editing comments, commit messages, ...
  :config
  ;; Remember visited orgs and repos across sessions
  ;; (add-to-list 'savehist-additional-variables 'consult-gh--known-orgs-list)
  ;; (add-to-list 'savehist-additional-variables 'consult-gh--known-repos-list)
  ;; Enable default keybindings (e.g. for commenting on issues, prs, ...)
  (consult-gh-enable-default-keybindings))


;;; Install `consult-gh-embark' for embark actions
(use-package consult-gh-embark
  :config
  :ensure t
  :defer t
  :init
  (consult-gh-embark-mode +1))


;;; Install `consult-gh-forge' for forge actions
(use-package consult-gh-forge
  :config
  :ensure t
  :defer t
  :init
  (consult-gh-forge-mode +1)
  (setq consult-gh-forge-timeout-seconds 20))

(use-package magit-log-file)


(provide 'init-vc)
;;; init-vc.el ends here
