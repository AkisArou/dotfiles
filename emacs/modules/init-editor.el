;;; init-editor.el --- Editor Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Editor configuration

;;; Code:

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


;; For a more ergonomic Emacs and `dape' experience
(use-package repeat
  :custom
  (repeat-mode +1))



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


;;; HL-TODO
(use-package hl-todo
  :vc (:url "https://github.com/tarsius/hl-todo.git")
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  (global-hl-todo-mode))



;; UNDO TREE
;; The `undo-tree' package provides an advanced and visual way to
;; manage undo history. It allows you to navigate and visualize your
;; undo history as a tree structure, making it easier to manage
;; changes in your buffers.
(use-package undo-tree
  :straight t
  :init
  ;; Persistent undo history
  (setq undo-tree-auto-save-history t
		undo-tree-visualizer-timestamps t
		undo-tree-visualizer-diff t

		;; Undo limits are in BYTES, not entries
		;; These values allow very deep undo history
		undo-limit 800000
		undo-strong-limit 12000000
		undo-outer-limit 120000000

		;; Where undo-tree stores history files
		undo-tree-history-directory-alist
		`(("." . ,(expand-file-name "undo" "~/.cache/emacs/"))))

  ;; Ensure the undo directory exists
  (make-directory (expand-file-name "undo" "~/.cache/emacs/") t)

  :config
  (global-undo-tree-mode 1))


(provide 'init-editor)
;;; init-editor.el ends here
