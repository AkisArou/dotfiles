;;; init.el --- Emacs-Kick --- A feature rich Emacs config for (neo)vi(m)mers -*- lexical-binding: t; -*-

;; performance
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


;; In Emacs, a package is a collection of Elisp code that extends the editor's functionality,
;; much like plugins do in Neovim. We need to import this package to add package archives.
(require 'package)
(require 'project)

;; Add MELPA (Milkypostman's Emacs Lisp Package Archive) to the list of package archives.
;; This allows you to install packages from this widely-used repository, similar to how
;; pip works for Python or npm for Node.js. While Emacs comes with ELPA (Emacs Lisp
;; Package Archive) configured by default, which contains packages that meet specific
;; licensing criteria, MELPA offers a broader range of packages and is considered the
;; standard for Emacs users. You can also add more package archives later as needed.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Add lisp/ directory to Emacs' load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Define a global customizable variable `ek-use-nerd-fonts' to control the use of
;; Nerd Fonts symbols throughout the configuration. This boolean variable allows
;; users to easily enable or disable the use of symbols from Nerd Fonts, providing
;; flexibility in appearance settings. By setting it to `t', we enable Nerd Fonts
;; symbols; setting it to `nil' would disable them.
(defcustom ek-use-nerd-fonts t
  "Configuration for using Nerd Fonts Symbols."
  :type 'boolean
  :group 'appearance)


;; From now on, you'll see configurations using the `use-package` macro, which
;; allows us to organize our Emacs setup in a modular way. These configurations
;; look like this:
;;
;; (use-package some-package
;;   :ensure t     ;; Ensure the package is installed (used with package.el).
;;   :straight t   ;; Use straight.el to install and manage this package.
;;   :config       ;; Configuration settings for the package.
;;   ;; Additional settings can go here.
;; )
;;
;; This approach simplifies package management, enabling us to easily control
;; both built-in (first-party) and external (third-party) packages. While Emacs
;; is a vast and powerful editor, using `use-package`—especially in combination
;; with `straight.el`—helps streamline our configuration for better organization,
;; reproducibility, and customization. As we proceed, you'll see smaller
;; `use-package` declarations for specific packages, which will help us enable
;; the desired features and improve our workflow.

;;; EMACS
;;  This is biggest one. Keep going, plugins (oops, I mean packages) will be shorter :)
(use-package emacs
  :ensure nil
  :custom
  (auto-save-default nil)
  (column-number-mode t)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (delete-selection-mode 1)
  (display-line-numbers-type 'relative)
  (global-auto-revert-non-file-buffers t)
  (history-length 25)
  (ispell-dictionary "en_US")
  (make-backup-files nil)
  (confirm-kill-processes nil)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (ring-bell-function 'ignore)
  (split-width-threshold 300)
  (switch-to-buffer-obey-display-actions t)
  (tab-width 4)
  (treesit-font-lock-level 4)
  (truncate-lines t)
  (use-dialog-box nil)
  (use-short-answers t)
  (warning-minimum-level :emergency)


  :hook
  (prog-mode . display-line-numbers-mode)

  :config
  ;; Skip special buffers when cycling with [b and ]b
  (defun skip-these-buffers (_window buffer _bury-or-kill)
	(string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)

  ;; Custom file
  (setq custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage)

  ;; Pretty vertical divider
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  ;; Centered cursor scrolling behavior
  (setq scroll-preserve-screen-position t
		scroll-conservatively 0
		maximum-scroll-margin 0.5
		scroll-margin 99999)

  (electric-pair-mode 1)          ;; Auto-insert matching delimiters
  (superword-mode 1)              ;; Treats words with dash/underscore as single words

  ;; ───────────────────────────────────────────────────────────────
  ;; ★ 2-space indentation for JS, TS, TSX, JSON, YAML
  ;; ───────────────────────────────────────────────────────────────

  ;; Modes that should always use spaces + 2-width tabs
  (dolist (hook '(js-ts-mode-hook
				  json-ts-mode-hook
				  typescript-ts-mode-hook
				  tsx-ts-mode-hook
				  yaml-ts-mode-hook))
	(add-hook hook
			  (lambda ()
				(setq-local indent-tabs-mode nil)
				(setq-local tab-width 2))))

  ;; Mode-specific indent offsets
  (setq js-indent-level 2)
  (setq typescript-ts-mode-indent-offset 2)
  (setq tsx-indent-offset 2)
  (setq json-ts-mode-indent-offset 2)
  (setq yaml-indent-offset 2)

  ;; ───────────────────────────────────────────────────────────────

  :init
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (global-hl-line-mode 1)
  (global-hl-line-mode -1)
  (global-auto-revert-mode 1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (file-name-shadow-mode 1)

  ;; Default file encoding
  (modify-coding-system-alist 'file "" 'utf-8))

(add-hook 'minibuffer-setup-hook (lambda ()
								   (local-set-key (kbd "C-c") 'abort-minibuffers)))

(defun local/postprocess-compilation-buffer ()
  "Clear compilation buffer if terminal clear sequences appear, then apply ANSI colors."
  (let ((inhibit-read-only t))
	(goto-char compilation-filter-start)
	;; Check for clear screen sequences at the start of new output
	(when (looking-at "\033\\[2J\033\\[3J\033\\[H")
	  ;; Clear the entire buffer
	  (delete-region (point-min) (point-max)))
	;; Apply ANSI colors to the new output
	(ansi-color-apply-on-region compilation-filter-start (point-max))))

(add-hook 'compilation-filter-hook 'local/postprocess-compilation-buffer)

;;; WHITESPACE
(use-package whitespace
  :ensure nil
  :defer t
  :hook (before-save . whitespace-cleanup))


;;; WINDOW
;; This section configures window management in Emacs, enhancing the way buffers
;; are displayed for a more efficient workflow. The `window' use-package helps
;; streamline how various buffers are shown, especially those related to help,
;; diagnostics, and completion.
;;
;; Note: I have left some commented-out code below that may facilitate your
;; Emacs journey later on. These configurations can be useful for displaying
;; other types of buffers in side windows, allowing for a more organized workspace.
(use-package window
  :ensure nil       ;; This is built-in, no need to fetch it.
  :defer t
  :custom
  (display-buffer-alist
   '(
	 ;; ("\\*.*e?shell\\*"
	 ;;  (display-buffer-in-side-window)
	 ;;  (window-height . 0.25)
	 ;;  (side . bottom)
	 ;;  (slot . -1))

	 ;; ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
	 ;;  (display-buffer-in-side-window)
	 ;;  (window-height . 0.25)
	 ;;  (side . bottom)
	 ;;  (slot . 0))

	 ;; Example configuration for the LSP help buffer,
	 ;; keeps it always on bottom using 25% of the available space:
	 ("\\*\\(lsp-help\\)\\*"
	  (display-buffer-in-side-window)
	  (window-height . 0.25)
	  (side . bottom)
	  (slot . 0))
	 )))


;;; CLIPBOARD
(use-package clipboard)

(use-package clipetty
  :ensure t
  :defer t
  :hook ((after-init . (lambda ()
						 (when (getenv "SSH_TTY")
						   (global-clipetty-mode 1))))))


;;; POSFRAME
(use-package posframe
  :ensure t)

;;; DIRED
;; In Emacs, the `dired' package provides a powerful and built-in file manager
;; that allows you to navigate and manipulate files and directories directly
;; within the editor. If you're familiar with `oil.nvim', you'll find that
;; `dired' offers similar functionality natively in Emacs, making file
;; management seamless without needing external plugins.

;; This configuration customizes `dired' to enhance its usability. The settings
;; below specify how file listings are displayed, the target for file operations,
;; and associations for opening various file types with their respective applications.
;; For example, image files will open with `feh', while audio and video files
;; will utilize `mpv'.
;; (use-package dired
;;   :ensure nil
;;   :init
;;   (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
;;		;; don't prompt to revert, just do it
;;		dired-auto-revert-buffer #'dired-buffer-stale-p
;;		;; Always copy/delete recursively
;;		dired-recursive-copies  'always
;;		dired-recursive-deletes 'top
;;		;; Ask whether destination dirs should get created when copying/removing files.
;;		dired-create-destination-dirs 'ask
;;		;; Screens are larger nowadays, we can afford slightly larger thumbnails
;;		image-dired-thumb-size 150)
;;   :custom
;;   (dired-listing-switches "-lah --group-directories-first")
;;   (dired-dwim-target t)
;;   (dired-guess-shell-alist-user
;;    '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open")
;;	 ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open")
;;	 (".*" "open" "xdg-open")))
;;   (dired-kill-when-opening-new-dired-buffer t)
;;   :config
;;   (add-hook 'dired-mode-hook #'dired-hide-details-mode)
;;   (add-hook 'dired-mode-hook #'dired-omit-mode)

;;   ;; Evil integration
;;   (with-eval-after-load 'evil
;;	(add-hook 'dired-mode-hook
;;				(lambda ()
;;				(evil-normalize-keymaps)
;;				(evil-define-key 'normal dired-mode-map
;;					(kbd "%") 'dired-create-empty-file
;;					(kbd "C-x") #'wdired-change-to-wdired-mode
;;					(kbd "C-e") 'dired-find-file
;;					(kbd "C-f") 'dired-up-directory)))))

;; (use-package dired-x
;;   :ensure nil
;;   :hook (dired-mode . dired-omit-mode)
;;   :config
;;   (setq dired-omit-verbose nil
;;		dired-omit-files
;;		(concat dired-omit-files
;;				"\\|^\\.DS_Store\\'"
;;				"\\|^flycheck_.*"
;;				"\\|^\\.project\\(?:ile\\)?\\'"
;;				"\\|^\\.\\(?:svn\\|git\\)\\'"
;;				"\\|^\\.ccls-cache\\'"
;;				"\\|\\(?:\\.js\\)?\\.meta\\'"
;;				"\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
;;   ;; Disable the prompt about whether I want to kill the Dired buffer for a
;;   ;; deleted directory. Of course I do!
;;   (setq dired-clean-confirm-killing-deleted-buffers nil)
;;   ;; Let OS decide how to open certain files
;;   (when-let (cmd (cond ((featurep :system 'macos) "open")
;;						 ((featurep :system 'linux) "xdg-open")
;;						 ((featurep :system 'windows) "start")))
;;	(setq dired-guess-shell-alist-user
;;			`(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
;;			("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
;;			("\\.\\(?:xcf\\)\\'" ,cmd)
;;			("\\.csv\\'" ,cmd)
;;			("\\.tex\\'" ,cmd)
;;			("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
;;			("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
;;			("\\.html?\\'" ,cmd)
;;			("\\.md\\'" ,cmd)))))


;;; DIRVISH
(use-package dirvish
  :ensure t
  :defer t
  :config
  (set-face-attribute 'dirvish-hl-line nil
					  :foreground 'unspecified
					  :background 'unspecified
					  :weight 'normal)

  (set-face-attribute 'highlight nil :background "#000000" :foreground 'unspecified)

  (setq dirvish-default-sort 'directories-first)
  (setq dirvish-reuse-session nil)
  (setq dirvish-path-separators '("~" "/" "/"))
  (setq dired-listing-switches "-AX --group-directories-first")

  :init
  (dirvish-override-dired-mode)

  (add-hook 'dired-mode-hook
			(lambda ()
			  (evil-normalize-keymaps)

			  ;; ------------------------------
			  ;; MAIN KEYBINDINGS (NORMAL MODE)
			  ;; ------------------------------
			  (evil-define-key 'normal dirvish-mode-map
				(kbd "%")   #'dired-create-empty-file
				(kbd "C-x") #'wdired-change-to-wdired-mode
				(kbd "C-e") #'dired-find-file
				(kbd "C-f") #'dired-up-directory

				(kbd "?")   #'dirvish-dispatch
				(kbd "q")   #'dirvish-quit
				(kbd "b")   #'dirvish-quick-access
				(kbd "p")   #'dirvish-yank
				(kbd "S")   #'dirvish-quicksort
				(kbd "F")   #'dirvish-layout-toggle
				(kbd "z")   #'dirvish-history-jump
				(kbd "gr")  #'project-dired
				(kbd "gh")  #'dirvish-subtree-up
				(kbd "gl")  #'dirvish-subtree-toggle

				(kbd "TAB") #'dirvish-subtree-toggle

				(kbd "M-b") #'dirvish-history-go-backward
				(kbd "M-f") #'dirvish-history-go-forward
				(kbd "M-n") #'dirvish-narrow
				(kbd "M-m") #'dirvish-mark-menu
				(kbd "M-s") #'dirvish-setup-menu
				(kbd "M-e") #'dirvish-emerge-menu)

			  ;; ------------------------------
			  ;; NORMAL + VISUAL (ng in Doom)
			  ;; ------------------------------
			  (evil-define-key '(normal visual) dirvish-mode-map
				(kbd "f")   #'dirvish-file-info-menu
				(kbd "S")   #'dirvish-quicksort  ;; duplicated in Doom
				(kbd "TAB") #'dirvish-subtree-toggle
				(kbd "M-b") #'dirvish-history-go-backward
				(kbd "M-f") #'dirvish-history-go-forward
				(kbd "M-n") #'dirvish-narrow
				(kbd "M-m") #'dirvish-mark-menu
				(kbd "M-s") #'dirvish-setup-menu
				(kbd "M-e") #'dirvish-emerge-menu)

			  ;; ------------------------------
			  ;; MOTION MAP BINDINGS
			  ;; ------------------------------
			  (evil-define-key 'motion dirvish-mode-map
				(kbd "[h") #'dirvish-history-go-backward
				(kbd "]h") #'dirvish-history-go-forward
				(kbd "[e") #'dirvish-emerge-next-group
				(kbd "]e") #'dirvish-emerge-previous-group)

			  ;; ------------------------------
			  ;; PREFIX "y" (YANK)
			  ;; ------------------------------
			  (define-prefix-command 'dirvish-yank-prefix)
			  (define-key dirvish-mode-map (kbd "y") 'dirvish-yank-prefix)

			  (evil-define-key 'normal dirvish-yank-prefix
				(kbd "l") #'dirvish-copy-file-true-path
				(kbd "n") #'dirvish-copy-file-name
				(kbd "p") #'dirvish-copy-file-path
				(kbd "r") #'dirvish-copy-remote-path
				(kbd "y") #'dired-do-copy)

			  ;; ------------------------------
			  ;; PREFIX "s" (SYMLINKS)
			  ;; ------------------------------
			  (define-prefix-command 'dirvish-symlink-prefix)
			  (define-key dirvish-mode-map (kbd "s") 'dirvish-symlink-prefix)

			  (evil-define-key 'normal dirvish-symlink-prefix
				(kbd "s") #'dirvish-symlink
				(kbd "S") #'dirvish-relative-symlink
				(kbd "h") #'dirvish-hardlink))))

;;; ISEARCH
;; In this configuration, we're setting up isearch, Emacs's incremental search feature.
;; Since we're utilizing Vim bindings, keep in mind that classic Vim search commands
;; (like `/' and `?') are not bound in the same way. Instead, you'll need to use
;; the standard Emacs shortcuts:
;; - `C-s' to initiate a forward search
;; - `C-r' to initiate a backward search
;; The following settings enhance the isearch experience:
(use-package isearch
  :ensure nil                                  ;; This is built-in, no need to fetch it.
  :defer t
  :config
  (setq isearch-lazy-count t)                  ;; Enable lazy counting to show current match information.
  (setq lazy-count-prefix-format "(%s/%s) ")   ;; Format for displaying current match count.
  (setq lazy-count-suffix-format nil)          ;; Disable suffix formatting for match count.
  (setq search-whitespace-regexp ".*?"))        ;; Allow searching across whitespace.



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

;;; ELDOC
;; Eldoc provides helpful inline documentation for functions and variables
;; in the minibuffer, enhancing the development experience. It can be particularly useful
;; in programming modes, as it helps you understand the context of functions as you type.
;; This package is built-in, so there's no need to fetch it separately.
;; The following line enables Eldoc globally for all buffers.
(use-package eldoc
  :ensure nil                                ;; This is built-in, no need to fetch it.
  :defer t
  :config
  (setq eldoc-idle-delay 0)                  ;; Automatically fetch doc help
  (setq eldoc-echo-area-use-multiline-p nil) ;; We use the "K" floating help instead
  ;; set to t if you want docs on the echo area
  (setq eldoc-echo-area-display-truncation-message nil)
  :init
  (global-eldoc-mode))


;;; FLYCHECK
(use-package flycheck
  :ensure t
  :defer t
  :config
  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.25)
  :init (global-flycheck-mode))

(with-eval-after-load 'flycheck
  (require 'flycheck-posframe)
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

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
  (setq org-directory "~/org/"))



;;; WHICH-KEY
;; `which-key' is an Emacs package that displays available keybindings in a
;; popup window whenever you partially type a key sequence. This is particularly
;; useful for discovering commands and shortcuts, making it easier to learn
;; Emacs and improve your workflow. It helps users remember key combinations
;; and reduces the cognitive load of memorizing every command.
(use-package which-key
  :ensure nil     ;; This is built-in, no need to fetch it.
  :defer t        ;; Defer loading Which-Key until after init.
  :hook
  (after-init . which-key-mode)) ;; Enable which-key mode after initialization.


;;; ==================== EXTERNAL PACKAGES ====================
;;
;; From this point onward, all configurations will be for third-party packages
;; that enhance Emacs' functionality and extend its capabilities.

;;; VERTICO
;; Vertico enhances the completion experience in Emacs by providing a
;; vertical selection interface for both buffer and minibuffer completions.
;; Unlike traditional minibuffer completion, which displays candidates
;; in a horizontal format, Vertico presents candidates in a vertical list,
;; making it easier to browse and select from multiple options.
;;
;; In buffer completion, `switch-to-buffer' allows you to select from open buffers.
;; Vertico streamlines this process by displaying the buffer list in a way that
;; improves visibility and accessibility. This is particularly useful when you
;; have many buffers open, allowing you to quickly find the one you need.
;;
;; In minibuffer completion, such as when entering commands or file paths,
;; Vertico helps by showing a dynamic list of potential completions, making
;; it easier to choose the correct one without typing out the entire string.
(use-package vertico
  :ensure t
  :straight t
  :defer t
  :hook
  (after-init . vertico-mode)           ;; Enable vertico after Emacs has initialized.
  :custom
  (vertico-count 10)                    ;; Number of candidates to display in the completion list.
  (vertico-resize nil)                  ;; Disable resizing of the vertico minibuffer.
  (vertico-cycle nil)                   ;; Do not cycle through candidates when reaching the end of the list.
  :config
  (vertico-buffer-mode)

  (defun my/vertico-backward-kill-word ()
	(interactive)
	(if (bound-and-true-p evil-local-mode)
		(evil-delete-backward-word 1)
	  (backward-kill-word 1)))

  (define-key vertico-map (kbd "C-w") #'my/vertico-backward-kill-word)
  (define-key vertico-map (kbd "C-e") #'vertico-exit)
  (define-key vertico-map (kbd "C-d") #'vertico-scroll-up)
  (define-key vertico-map (kbd "C-u") #'vertico-scroll-down)

  ;; Customize the display of the current candidate in the completion list.
  ;; This will prefix the current candidate with “» ” to make it stand out.
  ;; Reference: https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
			  (lambda (orig cand prefix suffix index _start)
				(setq cand (funcall orig cand prefix suffix index _start))
				(concat
				 (if (= vertico--index index)
					 (propertize "» " 'face '(:foreground "#80adf0" :weight bold))
				   "  ")
				 cand))))

(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

(use-package orderless
  :straight t
  :ensure t
  :commands (orderless-filter))

(use-package flx-rs
  :ensure t
  :straight
  (flx-rs
   :repo "jcs-elpa/flx-rs"
   :fetcher github
   :files (:defaults "bin"))
  :config
  (flx-rs-load-dyn))

(use-package fzf-native
  :straight (fzf-native
			 :repo "dangduc/fzf-native"
			 :host github
			 :files (:defaults "bin"))
  :config
  (fzf-native-load-dyn))


(use-package fussy
  :ensure t
  :straight
  (fussy :type git :host github :repo "jojojames/fussy")
  :config
  (setq fussy-filter-fn 'fussy-filter-orderless-flex)
  (setq fussy-use-cache t)
  (setq fussy-compare-same-score-fn 'fussy-histlen->strlen<)

  (with-eval-after-load 'corfu
	(advice-add 'corfu--capf-wrapper :before #'fussy-wipe-cache)
	(add-hook 'corfu-mode-hook
			  (lambda ()
				(setq-local fussy-max-candidate-limit 5000
							fussy-default-regex-fn 'fussy-pattern-first-letter
							fussy-prefer-prefix nil))))
  (fussy-setup))

(defun my/fussy-use-flx-rs ()
  (setq-local fussy-score-fn #'fussy-flx-rs-score))

(defun my/fussy-use-fzf-native ()
  (setq-local fussy-score-fn #'fussy-fzf-native-score))

(add-hook 'corfu-mode-hook #'my/fussy-use-flx-rs)
(add-hook 'minibuffer-setup-hook #'my/fussy-use-fzf-native)


;;; MARGINALIA
;; Marginalia enhances the completion experience in Emacs by adding
;; additional context to the completion candidates. This includes
;; helpful annotations such as documentation and other relevant
;; information, making it easier to choose the right option.
(use-package marginalia
  :ensure t
  :straight t
  :defer t
  :hook
  (after-init . marginalia-mode))


;;; CONSULT
;; Consult provides powerful completion and narrowing commands for Emacs.
;; It integrates well with other completion frameworks like Vertico, enabling
;; features like previews and enhanced register management. It's useful for
;; navigating buffers, files, and xrefs with ease.
(use-package consult
  :ensure t
  :straight t
  :defer t
  :init
  ;; Enhance register preview with thin lines and no mode line.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult for xref locations with a preview feature.
  (setq xref-show-xrefs-function #'consult-xref
		xref-show-definitions-function #'consult-xref))

(use-package consult-flycheck
  :ensure t
  :straight t
  :defer t)

;;; EMBARK
;; Embark provides a powerful contextual action menu for Emacs, allowing
;; you to perform various operations on completion candidates and other items.
;; It extends the capabilities of completion frameworks by offering direct
;; actions on the candidates.
;; Just `<leader> .' over any text, explore it :)
(use-package embark
  :ensure t
  :straight t
  :defer t)


;;; EMBARK-CONSULT
;; Embark-Consult provides a bridge between Embark and Consult, ensuring
;; that Consult commands, like previews, are available when using Embark.
(use-package embark-consult
  :ensure t
  :straight t
  :defer t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)) ;; Enable preview in Embark collect mode.


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

;;; HL-TODO
(use-package hl-todo
  :vc (:url "https://github.com/tarsius/hl-todo.git")
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  (global-hl-todo-mode))


(use-package corfu
  :ensure t
  :straight t
  :defer t
  :custom
  ;; Enable auto-completion like VSCode
  (corfu-auto t)
  (corfu-auto-delay 0.1)              ;; Show completions quickly like VSCode
  (corfu-auto-prefix 1)               ;; Start completing after 1 character
  (corfu-quit-no-match t)             ;; Quit if no match (like VSCode)
  (corfu-quit-at-boundary 'separator) ;; Allow multi-part filtering
  (corfu-scroll-margin 5)
  (corfu-max-width 50)
  (corfu-min-width 50)
  (corfu-popupinfo-delay 0.3)         ;; Show info popup quickly
  (corfu-preselect 'first)            ;; Preselect best match (already sorted by fzf score)
  (corfu-preview-current t)           ;; Show preview of selected candidate
  (corfu-cycle nil)                   ;; Don't cycle through candidates
  (corfu-on-exact-match 'insert)      ;; Insert exact matches immediately


  :config
  (if ek-use-nerd-fonts
	  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

  :bind
  (:map corfu-map
		("RET" . nil)
		("C-e" . corfu-insert)        ;; Keep your existing binding
		("C-n" . corfu-next)          ;; Ctrl-n for next
		("C-p" . corfu-previous))     ;; Ctrl-p for previous

  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode t))

;; Optional: Configure completion categories for better matching
(setq completion-category-overrides
	  '((buffer (styles fussy basic))
		(file (styles fussy partial-completion))
		(project-file (styles fussy))
		(lsp-capf (styles fussy basic))))

;; Add extensions
(use-package cape
  :ensure t
  :defer t
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )


;;; NERD-ICONS-CORFU
;; Provides Nerd Icons to be used with CORFU.
(use-package nerd-icons-corfu
  :if ek-use-nerd-fonts
  :ensure t
  :straight t
  :after (:all corfu))

;;; MASON
(use-package mason
  :ensure t
  :config
  ;; List of packages to ensure
  (defvar my-mason-packages
	'("vtsls"
	  "lua-language-server"
	  "json-lsp"
	  "html-lsp"
	  "css-lsp"
	  "gopls"
	  "tailwindcss-language-server"
	  "cssmodules-language-server"
	  "docker-language-server"
	  "docker-compose-language-service"
	  "yaml-language-server"
	  "clangd"
	  "mdx_analyzer"
	  "prettierd"
	  "taplo"))

  ;; Initialize Mason and install packages
  (mason-ensure
   (lambda ()
	 (dolist (pkg my-mason-packages)
	   (ignore-errors (mason-install pkg))))))


;;; LSP
;; Emacs comes with an integrated LSP client called `eglot', which offers basic LSP functionality.
;; However, `eglot' has limitations, such as not supporting multiple language servers
;; simultaneously within the same buffer (e.g., handling both TypeScript, Tailwind and ESLint
;; LSPs together in a React project). For this reason, the more mature and capable
;; `lsp-mode' is included as a third-party package, providing advanced IDE-like features
;; and better support for multiple language servers and configurations.
;;
;; NOTE: To install or reinstall an LSP server, use `M-x install-server RET`.
;;       As with other editors, LSP configurations can become complex. You may need to
;;       install or reinstall the server for your project due to version management quirks
;;       (e.g., asdf or nvm) or other issues.
;;       Fortunately, `lsp-mode` has a great resource site:
;;       https://emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :ensure t
  :straight t
  :defer t
  :hook (;; Replace XXX-mode with concrete major mode (e.g. python-mode)
		 (lsp-mode . lsp-enable-which-key-integration)  ;; Integrate with Which Key
		 ((js-mode                                      ;; Enable LSP for JavaScript
		   tsx-ts-mode                                  ;; Enable LSP for TSX
		   typescript-ts-base-mode                      ;; Enable LSP for TypeScript
		   css-mode                                     ;; Enable LSP for CSS
		   js-ts-mode                                   ;; Enable LSP for JavaScript (TS mode)
		   go-ts-mode                                   ;; Enable LSP for Go
		   json-ts-mode                                 ;; Enable LSP for JSON
		   web-mode) . lsp-deferred))                   ;; Enable LSP for Web (HTML)
  :commands lsp
  :config
  (set-face-attribute 'lsp-face-highlight-textual nil
					  :background "#292e42"
					  :weight 'normal)
  :custom
  (lsp-keymap-prefix "C-c l")                           ;; Set the prefix for LSP commands.
  (lsp-inlay-hint-enable nil)                           ;; Usage of inlay hints.
  (lsp-completion-provider :none)                       ;; Disable the default completion provider.
  (lsp-session-file (locate-user-emacs-file ".lsp-session")) ;; Specify session file location.
  (lsp-log-io nil)                                      ;; Disable IO logging for speed.
  (lsp-idle-delay 0)                                    ;; Set the delay for LSP to 0 (debouncing).
  (lsp-keep-workspace-alive t)                          ;; Keep the workspace alive.
  ;; Core settings
  (lsp-enable-xref t)                                   ;; Enable cross-references.
  (lsp-auto-configure t)                                ;; Automatically configure LSP.
  (lsp-enable-links nil)                                ;; Disable links.
  (lsp-eldoc-enable-hover t)                            ;; Enable ElDoc hover.
  (lsp-enable-file-watchers nil)                        ;; Disable file watchers.
  (lsp-enable-folding nil)                              ;; Disable folding.
  (lsp-enable-imenu t)                                  ;; Enable Imenu support.
  (lsp-enable-indentation nil)                          ;; Disable indentation.
  (lsp-enable-on-type-formatting nil)                   ;; Disable on-type formatting.
  (lsp-enable-suggest-server-download t)                ;; Enable server download suggestion.
  (lsp-enable-symbol-highlighting t)                    ;; Enable symbol highlighting.
  (lsp-enable-text-document-color t)                    ;; Enable text document color.
  ;; Modeline settings
  (lsp-modeline-code-actions-enable nil)                ;; Keep modeline clean.
  (lsp-modeline-diagnostics-enable nil)                 ;; Use `flycheck' instead.
  (lsp-modeline-workspace-status-enable t)              ;; Display "LSP" in the modeline when enabled.
  (lsp-signature-doc-lines 1)                           ;; Limit echo area to one line.
  (lsp-eldoc-render-all t)                              ;; Render all ElDoc messages.
  ;; Completion settings
  (lsp-completion-enable t)                             ;; Enable completion.
  (lsp-completion-enable-additional-text-edit t)        ;; Enable additional text edits for completions.
  (lsp-enable-snippet nil)                              ;; Disable snippets
  (lsp-completion-show-kind t)                          ;; Show kind in completions.
  ;; Lens settings
  (lsp-lens-enable nil)                                 ;; Disable lens support.
  ;; Semantic settings
  (lsp-semantic-tokens-enable nil))                     ;; Disable semantic tokens.

(setq lsp-headerline-breadcrumb-enable nil)

(use-package lsp-vtsls
  :straight (lsp-vtsls
			 :type git
			 :host github
			 :repo "sdvcrx/lsp-vtsls")
  :after lsp-mode
  :config
  ;; core vtsls settings
  (setq
   lsp-eldoc-render-all t
   lsp-vtsls-server-side-fuzzy-match t
   lsp-vtsls-entries-limit 5000
   lsp-vtsls-auto-use-workspace-tsdk t)

  ;; formatting disabled
  (lsp-register-custom-settings
   '(("typescript.format.enable" nil)
	 ("javascript.format.enable" nil)))

  ;; TS/JS preferences
  (setq ts/js-preferences
		'(:quote
		  (:importModuleSpecifier "relative"
								  :quoteStyle "single"
								  :semi "remove")))

  (lsp-register-custom-settings
   `(("typescript.preferences" ,ts/js-preferences)
	 ("javascript.preferences" ,ts/js-preferences))))

(add-hook 'dired-mode-hook #'lsp-dired-mode)


;;; LSP Additional Servers
;; You can extend `lsp-mode' by integrating additional language servers for specific
;; technologies. For example, `lsp-tailwindcss' provides support for Tailwind CSS
;; classes within your HTML files. By using various LSP packages, you can connect
;; multiple LSP servers simultaneously, enhancing your coding experience across
;; different languages and frameworks.

;;; tailwindcss
(use-package lsp-tailwindcss
  :after lsp-mode
  :straight t
  :init

  ;; Validation
  (setq lsp-tailwindcss-validate t)
  (setq lsp-tailwindcss-add-on-mode t)

  ;; Lint rules
  (setq lsp-tailwindcss-lint-css-conflict "error"
		lsp-tailwindcss-lint-invalid-apply "error"
		lsp-tailwindcss-lint-invalid-screen "error"
		lsp-tailwindcss-lint-invalid-variant "error"
		lsp-tailwindcss-lint-invalid-config-path "error"
		lsp-tailwindcss-lint-invalid-tailwind-directive "error"
		lsp-tailwindcss-lint-recommended-variant-order "warning")

  ;; Equivalent: includeLanguages → NOT NEEDED in Emacs
  ;; (lsp-tailwindcss activates by major-mode & config rules)

  ;; classAttributes
  (setq lsp-tailwindcss-class-attributes
		["class" "className" "style" "classList"])

  ;; classFunctions
  (setq lsp-tailwindcss-class-functions
		["cn" "clsx" "tw" "tw.color" "tw.style"])

  ;; experimental.configFile
  (setq lsp-tailwindcss-experimental-config-file
		(ht
		 ("apps/client/assistant-prm-airport/back-office/src/styles.css"
		  ["apps/client/assistant-prm-airport/back-office/src/**"
		   "packages/assistant-prm-airport/frontend/coordinator/**"])

		 ("apps/client/assistant-prm-airport/agent/tailwind.config.ts"
		  ["apps/client/assistant-prm-airport/agent/**"])

		 ("apps/client/volunteer/back-office/tailwind.config.ts"
		  "packages/assistant-volunteer/**")

		 ("apps/website/nable/tailwind.config.ts"
		  "apps/website/nable/**")

		 ("packages/shared/react/heroui/src/index.css"
		  "packages/shared/react/heroui/**")))
  )


;;; oxlint / oxc_language_server
(defun my/oxlint-server-cmd ()
  "Return the path to the oxlint language server for the current project."
  (let ((server-path (expand-file-name "node_modules/.bin/oxc_language_server"
									   (lsp-workspace-root))))
	(if (file-executable-p server-path)
		server-path
	  (error "oxc_language_server not found in node_modules/.bin"))))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
	:new-connection (lsp-stdio-connection #'my/oxlint-server-cmd)
	:activation-fn (lsp-activate-on
					"javascript"
					"javascriptreact"
					"typescript"
					"typescriptreact")
	:server-id 'oxlint
	:add-on? t
	:priority -1)))

(defun my/oxlint-fix-all ()
  "Apply fixes to the current buffer using oxlint."
  (interactive)
  (when-let ((client (lsp--find-clients 'oxlint)))
	(lsp-request
	 "workspace/executeCommand"
	 `(:command "oxc.fixAll"
				:arguments [(:uri ,(lsp--buffer-uri))]))))


;; FORMAT-ALL
(use-package format-all
  :ensure t
  :defer t
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
				'(("JavaScript" . (prettierd))
				  ("JSX"        . (prettierd))
				  ("TypeScript" . (prettierd))
				  ("TSX"        . (prettierd))
				  ("JSON"       . (prettierd))
				  ("YAML"       . (prettierd)))))

(add-hook 'emacs-lisp-mode-hook 'format-all-mode)
(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)


;;; ELDOC-BOX
;; eldoc-box enhances the default Eldoc experience by displaying documentation in a popup box,
;; usually in a child frame. This makes it easier to read longer docstrings without relying on
;; the minibuffer. It integrates seamlessly with Eldoc and activates when Eldoc is active.
;; Useful for graphical Emacs; terminal users may want to fall back to `eldoc-box-display-at-point-mode'.
(use-package eldoc-box
  :ensure t
  :straight t
  :defer t)


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


(defun ek/terminal-cursor-update ()
  "Set cursor shape depending on Evil state in terminal."
  (when (not (display-graphic-p))
	(cond
	 ((evil-insert-state-p)
	  ;; vertical bar
	  (send-string-to-terminal "\e[6 q"))
	 ((evil-visual-state-p)
	  ;; block
	  (send-string-to-terminal "\e[2 q"))
	 (t
	  ;; normal, motion, replace, etc.
	  (send-string-to-terminal "\e[2 q")))))

(add-hook 'post-command-hook 'ek/terminal-cursor-update)


;; TAB-JUMP-OUT
(use-package tab-jump-out
  :straight (:host github
				   :repo "victorteokw/tab-jump-out")
  :ensure t
  :defer t
  :bind
  ("TAB" . tab-jump-out)  ;; optional, binds tab-jump-out to Tab
  :config
  (tab-jump-out-mode 1))


;; EVIL
;; The `evil' package provides Vim emulation within Emacs, allowing
;; users to edit text in a modal way, similar to how Vim
;; operates. This setup configures `evil-mode' to enhance the editing
;; experience.
(use-package evil
  :ensure t
  :straight t
  :defer t
  :hook
  (after-init . evil-mode)
  :init
  (setq evil-want-integration t)      ;; Integrate `evil' with other Emacs features (optional as it's true by default).
  (setq evil-want-keybinding nil)     ;; Disable default keybinding to set custom ones.
  (setq evil-want-C-u-scroll t)       ;; Makes C-u scroll
  (setq evil-symbol-word-search t)

  :config
  (setq evil-normal-state-cursor 'box
		evil-insert-state-cursor 'bar
		evil-visual-state-cursor 'box)

  (evil-set-undo-system 'undo-tree)   ;; Uses the undo-tree package as the default undo system

  (define-key evil-insert-state-map (kbd "C-e") nil)

  ;; Set the leader key to space for easier access to custom commands. (setq evil-want-leader t)
  (setq evil-leader/in-all-states t)  ;; Make the leader key available in all states.
  (setq evil-want-fine-undo nil)      ;; Default but keep it for reference

  (define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-s") 'evil-write-all)

  ;; Define the leader key as Space
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

  (evil-define-key 'normal 'global (kbd "<leader> h") 'evil-ex-nohighlight)
  (evil-define-key 'normal 'global (kbd "/") 'consult-line)

  ;; Keybindings for searching and finding files.
  (evil-define-key 'normal 'global (kbd "<leader> f f") 'project-find-file)
  (evil-define-key 'normal 'global (kbd "<leader> f s") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader> f S") 'consult-gitgrep)
  (evil-define-key 'normal 'global (kbd "<leader> f q") 'consult-compile-error)
  (evil-define-key 'normal 'global (kbd "<leader> f h") 'consult-info)
  (evil-define-key 'normal 'global (kbd "<leader> f m") 'consult-man)
  (evil-define-key 'normal 'global (kbd "<leader> f l") 'vertico-repeat)
  (evil-define-key 'normal 'global (kbd "<leader> /") 'consult-line)

  ;; Flycheck navigation
  (with-eval-after-load 'evil
	(define-key evil-window-map (kbd "C-d") #'flycheck-posframe-display-errors-manually))

  (evil-define-key 'normal 'global (kbd "<leader> f d") 'consult-flycheck)

  (defun my-show-flycheck-posframe-after (&rest _)
	"Show flycheck posframe after navigation."
	(run-with-idle-timer 0 nil #'flycheck-posframe-display-errors-manually))

  (defun my/flycheck-next-error-of-severity (severity)
	"Go to the next Flycheck error of SEVERITY, wrapping to the beginning if needed."
	(interactive)
	(my-show-flycheck-posframe-after)
	(let* ((errors (seq-filter (lambda (e) (eq (flycheck-error-level e) severity))
							   (flycheck-overlay-errors-in (point-min) (point-max))))
		   (next (seq-find (lambda (e) (> (flycheck-error-pos e) (point))) errors)))
	  (if next
		  (goto-char (flycheck-error-pos next))
		;; Wrap to the first error
		(if errors
			(goto-char (flycheck-error-pos (car errors)))
		  (message "No %s errors found" severity)))))

  (defun my/flycheck-previous-error-of-severity (severity)
	"Go to the previous Flycheck error of SEVERITY, wrapping to the end if needed."
	(interactive)
	(my-show-flycheck-posframe-after)
	(let* ((errors (seq-filter (lambda (e) (eq (flycheck-error-level e) severity))
							   (flycheck-overlay-errors-in (point-min) (point-max))))
		   (prev (car (last (seq-filter (lambda (e) (< (flycheck-error-pos e) (point))) errors)))))
	  (if prev
		  (goto-char (flycheck-error-pos prev))
		;; Wrap to the last error
		(if errors
			(goto-char (flycheck-error-pos (car (last errors))))
		  (message "No %s errors found" severity)))))

  ;; Specific wrappers
  (defun my/flycheck-next-error-only () (interactive) (my/flycheck-next-error-of-severity 'error))
  (defun my/flycheck-previous-error-only () (interactive) (my/flycheck-previous-error-of-severity 'error))
  (defun my/flycheck-next-warning-only () (interactive) (my/flycheck-next-error-of-severity 'warning))
  (defun my/flycheck-previous-warning-only () (interactive) (my/flycheck-previous-error-of-severity 'warning))

  ;; Keybindings
  (evil-define-key 'normal 'global (kbd "] e") 'my/flycheck-next-error-only)
  (evil-define-key 'normal 'global (kbd "[ e") 'my/flycheck-previous-error-only)
  (evil-define-key 'normal 'global (kbd "] w") 'my/flycheck-next-warning-only)
  (evil-define-key 'normal 'global (kbd "[ w") 'my/flycheck-previous-warning-only)
  (evil-define-key 'normal 'global (kbd "] d") 'flycheck-next-error)
  (evil-define-key 'normal 'global (kbd "[ d") 'flycheck-previous-error)

  ;; LSP
  (evil-define-key 'normal 'global (kbd "gra") 'lsp-execute-code-action)
  (evil-define-key 'normal 'global (kbd "grr") 'lsp-find-references)
  (evil-define-key 'normal 'global (kbd "grn") 'lsp-rename)
  (evil-define-key 'normal 'global (kbd "gI") 'lsp-find-implementation)
  (evil-define-key 'normal 'global (kbd "gra") 'lsp-execute-code-action)
  (evil-define-key 'normal 'global (kbd "<leader> l f") 'lsp-format-buffer)

  ;; Notmuch
  (evil-define-key 'normal 'global (kbd "<leader> m") 'notmuch)

  ;; Trigger completion at point in Evil insert mode
  (evil-define-key 'insert global-map (kbd "C-SPC") #'completion-at-point)
  (evil-define-key 'insert global-map (kbd "C-@")   #'completion-at-point)

  ;; Trigger completion at point in all minibuffer maps
  (dolist (map (list minibuffer-local-map
					 minibuffer-local-ns-map
					 minibuffer-local-completion-map
					 minibuffer-local-must-match-map))
	(define-key map (kbd "M-SPC") #'completion-at-point))

  ;; Dired commands for file management
  (evil-define-key 'normal 'global (kbd "<leader> x d") 'dired)
  (evil-define-key 'normal 'global (kbd "<leader> x j") 'dired-jump)
  (evil-define-key 'normal 'global (kbd "<leader> x f") 'find-file)

  ;; Diff-HL navigation for version control
  (evil-define-key 'normal 'global (kbd "] h") 'diff-hl-next-hunk) ;; Next diff hunk
  (evil-define-key 'normal 'global (kbd "[ h") 'diff-hl-pevious-hunk) ;; Previous diff hunk

  ;; File exploration
  (evil-define-key 'normal 'global (kbd "<leader> e") 'dired-jump)

  ;; Magit keybindings for Git integration
  (evil-define-key 'normal 'global (kbd "<leader> g s") 'magit-status)      ;; Open Magit status
  (evil-define-key 'normal 'global (kbd "<leader> g l") 'magit-log-current) ;; Show current log
  (evil-define-key 'normal 'global (kbd "<leader> g f") 'my/git-file-history) ;; Show current log for buffer
  (evil-define-key 'normal 'global (kbd "<leader> g d") 'magit-diff-buffer-file) ;; Show diff for the current file
  (evil-define-key 'normal 'global (kbd "<leader> g D") 'diff-hl-show-hunk) ;; Show diff for a hunk
  (evil-define-key 'normal 'global (kbd "<leader> g b") 'vc-annotate)       ;; Annotate buffer with version control info
  (evil-define-key 'normal 'global (kbd "<leader> g t") 'git-timemachine)       ;; Annotate buffer with version control info
  (evil-define-key 'normal 'global
	(kbd "<leader> g i")
	(lambda ()
	  (interactive)
	  (consult-gh-issue-list "nablesolutions/nable-solutions")))

  ;; Buffer management keybindings
  (defun my/project-kill-buffers-no-confirm ()
	"Kill all file buffers in the current project, skipping special buffers and LSP.
	Modified buffers are automatically saved before being killed."
	(interactive)
	(let ((project (project-current)))
	  (when project
		(dolist (buf (project-buffers project))
		  ;; Only handle normal file buffers
		  (when (and (buffer-file-name buf)
					 (not (string-match-p "^\\*" (buffer-name buf))))
			(with-current-buffer buf
			  ;; Save if modified
			  (when (buffer-modified-p)
				(save-buffer))
			  ;; Kill buffer
			  (kill-buffer buf)))))))

  (defun kill-other-buffers ()
	(interactive)
	(dolist (buf (delq (current-buffer) (buffer-list)))
	  (unless (string-prefix-p "*" (buffer-name buf))
		(kill-buffer buf))))

  (evil-define-key 'normal 'global (kbd "] b") 'switch-to-next-buffer) ;; Switch to next buffer
  (evil-define-key 'normal 'global (kbd "[ b") 'switch-to-prev-buffer) ;; Switch to previous buffer
  (evil-define-key 'normal 'global (kbd "<leader> b b") 'ibuffer) ;; Open Ibuffer
  (evil-define-key 'normal 'global (kbd "<leader> b d") 'kill-current-buffer) ;; Kill current buffer
  (evil-define-key 'normal 'global (kbd "<leader> b a") 'my/project-kill-buffers-no-confirm) ;; Kill project buffers
  (evil-define-key 'normal 'global (kbd "<leader> b o") #'kill-other-buffers) ;; Kill project buffers
  (evil-define-key 'normal 'global (kbd "<leader> b s") 'save-buffer) ;; Save buffer

  ;; Project management keybindings
  (defun get-project-root ()
	(when (fboundp 'projectile-project-root)
	  (projectile-project-root)))

  ;; Ripgrep the current word from project root
  (defun consult-ripgrep-region-or-word ()
	"Run `consult-ripgrep` on selected region in visual mode, or word at point otherwise."
	(interactive)
	(let ((search-text (if (use-region-p)
						   (buffer-substring-no-properties (region-beginning) (region-end))
						 (thing-at-point 'word t))))
	  ;; Deactivate the region so it doesn't interfere with the search
	  (deactivate-mark)
	  (consult-ripgrep (get-project-root) search-text)))

  (evil-define-key 'normal 'global (kbd "<leader> f e") 'consult-project-buffer) ;; Consult buffers
  (evil-define-key 'normal global-map (kbd "<leader> f w") #'consult-ripgrep-region-or-word)
  (evil-define-key 'visual global-map (kbd "<leader> f w") #'consult-ripgrep-region-or-word)
  (evil-define-key 'normal 'global (kbd "<leader> p p") 'project-switch-project) ;; Switch project
  (evil-define-key 'normal 'global (kbd "<leader> p D") 'project-dired) ;; Dired for project

  ;; Yank from kill ring
  ;; (evil-define-key 'normal 'global (kbd "P") 'consult-yank-from-kill-ring)
  (evil-define-key 'normal 'global (kbd "<leader> P") 'consult-yank-from-kill-ring)

  ;; Embark actions for contextual commands
  (evil-define-key 'normal 'global (kbd "<leader> .") 'embark-act)

  ;; Undo tree visualization
  (evil-define-key 'normal 'global (kbd "<leader> u") 'undo-tree-visualize)

  ;; Help keybindings
  ;; (evil-define-key 'normal 'global (kbd "<leader> h m") 'describe-mode) ;; Describe current mode
  ;; (evil-define-key 'normal 'global (kbd "<leader> h f") 'describe-function) ;; Describe function
  ;; (evil-define-key 'normal 'global (kbd "<leader> h v") 'describe-variable) ;; Describe variable
  ;; (evil-define-key 'normal 'global (kbd "<leader> h k") 'describe-key) ;; Describe key


  ;; Tab navigation
  (evil-define-key 'normal 'global (kbd "] t") 'tab-next) ;; Go to next tab
  (evil-define-key 'normal 'global (kbd "[ t") 'tab-previous) ;; Go to previous tab


  ;; Custom example. Formatting with prettier tool.
  (evil-define-key 'normal 'global (kbd "<leader> b f")
	(lambda ()
	  (interactive)
	  (shell-command (concat "prettier --write " (shell-quote-argument (buffer-file-name))))
	  (revert-buffer t t t)))

  ;; Enable evil mode
  (evil-mode 1))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "K") #'eldoc-box-help-at-point))


;;; EVIL-COMMENTARY
(use-package evil-commentary
  :after evil
  :straight t
  :config
  (evil-commentary-mode 1))

;;; EVIL-NUMBERS
(use-package evil-numbers
  :after evil
  :straight t
  :config
  (evil-define-key '(normal visual) 'global
	(kbd "C-a") 'evil-numbers/inc-at-pt
	(kbd "C-x") 'evil-numbers/dec-at-pt
	(kbd "g C-a") 'evil-numbers/inc-at-pt-incremental
	(kbd "g C-x") 'evil-numbers/dec-at-pt-incremental))

;;; EMACS-TMUX-NAVIGATOR
(use-package emacs-tmux-navigator
  :after evil
  :config
  (emacs-tmux-navigator-mode 1))

;; EVIL COLLECTION
;; The `evil-collection' package enhances the integration of
;; `evil-mode' with various built-in and third-party packages. It
;; provides a better modal experience by remapping keybindings and
;; commands to fit the `evil' style.
(use-package evil-collection
  :defer t
  :straight t
  :ensure t
  :custom
  (evil-collection-want-find-usages-bindings nil)
  (evil-collection-want-unimpaired-p nil)
  ;; Hook to initialize `evil-collection' when `evil-mode' is activated.
  :hook
  (evil-mode . evil-collection-init))


;; EVIL SURROUND
;; The `evil-surround' package provides text object surround
;; functionality for `evil-mode'. This allows for easily adding,
;; changing, or deleting surrounding characters such as parentheses,
;; quotes, and more.
;;
;; With this you can change 'hello there' with ci'" to have
;; "hello there" and cs"<p> to get <p>hello there</p>.
;; More examples here:
;; - https://github.com/emacs-evil/evil-surround?tab=readme-ov-file#examples
(use-package evil-surround
  :ensure t
  :straight t
  :after evil-collection
  :config
  (global-evil-surround-mode 1))


;;; EVIL-TEXTOBJ-ANYBLOCK
;;; SMART QUOTE TEXTOBJ (inner + outer, jumps to next quote on line)
(use-package evil-textobj-anyblock
  :straight (:host github :repo "noctuid/evil-textobj-anyblock")
  :after evil
  :config
  (defvar my/quote-pairs
	'(("'" . "'")
	  ("\"" . "\"")
	  ("`" . "`")
	  ("“" . "”"))
	"Supported quote delimiters.")

  ;; --------------------------------------------------------------------------
  ;; FIXED: TRUE inside-quote detection (works with multiple pairs per line)
  ;; --------------------------------------------------------------------------
  (defun my/find-containing-quote ()
	"Return (OPEN . CLOSE) if point is inside a quote pair on the line."
	(save-excursion
	  (let* ((line-beg (line-beginning-position))
			 (line-end (line-end-position))
			 ;; list of all (OPEN . CLOSE) sorted by position
			 (pairs '()))
		;; Collect all quote pairs on the current line
		(dolist (pair my/quote-pairs)
		  (let ((open (car pair))
				(close (cdr pair)))
			(save-excursion
			  (goto-char line-beg)
			  (while (search-forward open line-end t)
				(let ((op (match-beginning 0)))
				  (when (search-forward close line-end t)
					(push (cons op (match-beginning 0)) pairs)))))))
		;; Sort them by opening position
		(setq pairs (sort pairs (lambda (a b) (< (car a) (car b)))))
		;; Now check if point lies inside any
		(let ((pos (point)))
		  (catch 'return
			(dolist (p pairs)
			  (when (and (< (car p) pos) (< pos (cdr p)))
				(throw 'return p)))
			nil)))))

  ;; --------------------------------------------------------------------------
  ;; Find next quote pair on same line
  ;; --------------------------------------------------------------------------
  (defun my/find-next-quote (count)
	(let ((regex (regexp-opt (mapcar #'car my/quote-pairs)))
		  (line-end (line-end-position))
		  open-pos close-pos)
	  (catch 'return
		(dotimes (_ (or count 1))
		  (unless (re-search-forward regex line-end t)
			(throw 'return nil))
		  (setq open-pos (match-beginning 0))
		  (let* ((open-str (buffer-substring-no-properties open-pos (1+ open-pos)))
				 (close-str (cdr (assoc open-str my/quote-pairs))))
			(unless (search-forward close-str line-end t)
			  (throw 'return nil))
			(setq close-pos (match-beginning 0))))
		(cons open-pos close-pos))))

  ;; --------------------------------------------------------------------------
  ;; Build text object range
  ;; --------------------------------------------------------------------------
  (defun my/make-quote-range (count outer)
	(let* ((bounds (or (my/find-containing-quote)
					   (my/find-next-quote count))))
	  (unless bounds (user-error "No quote pair found"))
	  (let* ((open (car bounds))
			 (close (cdr bounds))
			 (beg (if outer open (1+ open)))
			 (end (if outer (1+ close) close)))
		(evil-range beg end 'exclusive))))

  ;; --------------------------------------------------------------------------
  ;; Text objects
  ;; --------------------------------------------------------------------------
  (evil-define-text-object my/inner-smart-quote
	(count &optional beg end type)
	(my/make-quote-range count nil))

  (evil-define-text-object my/outer-smart-quote
	(count &optional beg end type)
	(my/make-quote-range count t))

  ;; Bind q
  (define-key evil-inner-text-objects-map "q" #'my/inner-smart-quote)
  (define-key evil-outer-text-objects-map "q" #'my/outer-smart-quote)

  ;; Make i"/i'/i` work identically
  (dolist (key '("\"" "'" "`"))
	(define-key evil-inner-text-objects-map key #'my/inner-smart-quote)
	(define-key evil-outer-text-objects-map key #'my/outer-smart-quote)))

;; UNDO TREE
;; The `undo-tree' package provides an advanced and visual way to
;; manage undo history. It allows you to navigate and visualize your
;; undo history as a tree structure, making it easier to manage
;; changes in your buffers.
(use-package undo-tree
  :defer t
  :ensure t
  :straight t
  :hook
  (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
		undo-tree-visualizer-diff t
		undo-tree-auto-save-history t
		;; Increase undo limits to avoid losing history due to Emacs' garbage collection.
		;; These values can be adjusted based on your needs.
		;; 10X bump of the undo limits to avoid issues with premature
		;; Emacs GC which truncates the undo history very aggressively.
		undo-limit 800000                     ;; Limit for undo entries.
		undo-strong-limit 12000000            ;; Strong limit for undo entries.
		undo-outer-limit 120000000)           ;; Outer limit for undo entries.
  :config
  ;; Set the directory where `undo-tree' will save its history files.
  ;; This keeps undo history across sessions, stored in a cache directory.
  (setq undo-tree-history-directory-alist '(("." . "~/.cache/emacs/undo"))))


;;; DOTENV
;; A simple major mode to provide .env files with color highlighting
(use-package dotenv-mode
  :defer t
  :straight t
  :ensure t
  :config)


;;; DOOM MODELINE
;; The `doom-modeline' package provides a sleek, modern mode-line that is visually appealing
;; and functional. It integrates well with various Emacs features, enhancing the overall user
;; experience by displaying relevant information in a compact format.
(use-package doom-modeline
  :ensure t
  :straight t
  :defer t
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)  ;; Set the buffer file name style to just the buffer name (without path).
  (doom-modeline-project-detection 'project)           ;; Enable project detection for displaying the project name.
  (doom-modeline-buffer-name t)                        ;; Show the buffer name in the mode line.
  (doom-modeline-vcs-max-length 25)                    ;; Limit the version control system (VCS) branch name length to 25 characters.
  :config
  (if ek-use-nerd-fonts                                ;; Check if nerd fonts are being used.
	  (setq doom-modeline-icon t)                      ;; Enable icons in the mode line if nerd fonts are used.
	(setq doom-modeline-icon nil))                     ;; Disable icons if nerd fonts are not being used.
  :hook
  (after-init . doom-modeline-mode))

(with-eval-after-load 'doom-modeline
  (setq ring-bell-function #'ignore))

;;; NERD ICONS
;; The `nerd-icons' package provides a set of icons for use in Emacs. These icons can
;; enhance the visual appearance of various modes and packages, making it easier to
;; distinguish between different file types and functionalities.
(use-package nerd-icons
  :if ek-use-nerd-fonts                   ;; Load the package only if the user has configured to use nerd fonts.
  :ensure t                               ;; Ensure the package is installed.
  :straight t
  :defer t)                               ;; Load the package only when needed to improve startup time.


;;; NERD ICONS Dired
;; The `nerd-icons-dired' package integrates nerd icons into the Dired mode,
;; providing visual icons for files and directories. This enhances the Dired
;; interface by making it easier to identify file types at a glance.
(use-package nerd-icons-dired
  :if ek-use-nerd-fonts                   ;; Load the package only if the user has configured to use nerd fonts.
  :ensure t                               ;; Ensure the package is installed.
  :straight t
  :defer t                                ;; Load the package only when needed to improve startup time.
  :hook
  (dired-mode . nerd-icons-dired-mode))


;;; NERD ICONS COMPLETION
;; The `nerd-icons-completion' package enhances the completion interfaces in
;; Emacs by integrating nerd icons with completion frameworks such as
;; `marginalia'. This provides visual cues for the completion candidates,
;; making it easier to distinguish between different types of items.
(use-package nerd-icons-completion
  :if ek-use-nerd-fonts                   ;; Load the package only if the user has configured to use nerd fonts.
  :ensure t                               ;; Ensure the package is installed.
  :straight t
  :after (:all nerd-icons marginalia)     ;; Load after `nerd-icons' and `marginalia' to ensure proper integration.
  :config
  (nerd-icons-completion-mode)            ;; Activate nerd icons for completion interfaces.
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)) ;; Setup icons in the marginalia mode for enhanced completion display.


;;; DOOM THEMES
(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  ;; Load the theme
  (load-theme 'doom-tokyo-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  (custom-set-faces
   `(isearch ((t (:foreground "#16161e" :background "#bb9af7"))))
   `(lazy-highlight ((t (:foreground "#16161e" :background "#5e428f"))))
   `(dired-directory ((t (:foreground "#7aa2f7"))))
   `(show-paren-match ((t (:foreground "#7aa2f7"))))
   `(typescript-ts-jsx-attribute-face ((t (:foreground "#7dcfff"))))))

(set-face-foreground 'show-paren-mismatch nil)
(set-face-background 'show-paren-mismatch nil)


;;; COLORFUL-MODE
(use-package colorful-mode
  :ensure t
  :defer t
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))


;;; HIGHLIGHT VIM OP
(defface my/flash-face
  '((t (:background "#7b5fbf" :extend t)))
  "Face used to flash evil operator ranges.")

(defun my/flash-region (beg end)
  "Flash the region from BEG to END using a temporary overlay."
  (let ((ov (make-overlay beg end)))
	(overlay-put ov 'face 'my/flash-face)
	(run-at-time 0.08 nil #'delete-overlay ov)))

(defun my/evil-flash-motion (orig beg end &rest args)
  "Flash region affected by evil operator."
  (let ((result (apply orig beg end args)))
	;; absolutely ensure region is not active
	(deactivate-mark)
	;; flash without leaving any highlight behind
	(my/flash-region beg end)
	result))

;; Advice evil operators that use (beg end)
(dolist (op '(evil-yank
			  evil-yank-line))
  (advice-add op :around #'my/evil-flash-motion))


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


;;; shr
(setq shr-use-colors nil)


;;; YAML
(use-package yaml
  :ensure t
  :defer t)


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


;;; UTILITARY FUNCTION TO INSTALL EMACS-KICK
(defun ek/first-install ()
  "Install tree-sitter grammars and compile packages on first run..."
  (interactive)
  (switch-to-buffer "*Messages*")
  (message ">>> All required packages installed.")
  (message ">>> Configuring Emacs-Kick...")
  (message ">>> Configuring Tree Sitter parsers...")

  ;; Load treesit-auto package
  (require 'treesit-auto)

  ;; Set the languages you want to install (like your Neovim list)
  (setq treesit-auto-langs
		'(bash c css dockerfile html javascript jsdoc
			   lua markdown python toml
			   tsx typescript yaml))

  ;; Initialize treesit-auto (adds major mode remappings etc.)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1)

  ;; Install all grammars
  (treesit-auto-install-all)

  (message ">>> Configuring Nerd Fonts...")
  (require 'nerd-icons)
  (nerd-icons-install-fonts)

  (message ">>> Emacs-Kick installed! Press any key to close the installer and open Emacs normally. First boot will compile some extra stuff :)")
  (read-key)
  (kill-emacs))

(provide 'init)
;;; init.el ends here
