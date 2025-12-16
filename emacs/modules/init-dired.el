;;; init-dired.el --- Dired Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Dired configuration

;;; Code:

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

(provide 'init-dired)
;;; init-dired.el ends here
