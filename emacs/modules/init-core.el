;;; init-core.el --- Core Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Core configuration

;;; Code:

(put 'compile-command 'safe-local-variable
	 (lambda (v) (stringp v)))

;;; EMACS
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
  (read-process-output-max (* 1024 1024))
  (warning-minimum-level :emergency)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (window-sides-vertical t)

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

  (electric-pair-mode 1)
  (electric-indent-mode 1)
  (superword-mode 1)

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

  (defun my-typescript-smart-newline ()
	"Smart newline handling for TypeScript with proper brace indentation."
	(interactive)
	(let ((in-braces (and (eq (char-before) ?{)
						  (eq (char-after) ?}))))
	  (newline-and-indent)
	  (when in-braces
		(save-excursion
		  (forward-line 1)
		  (let ((target-col (save-excursion
							  (forward-line -2)
							  (back-to-indentation)
							  (current-column))))
			(beginning-of-line)
			(skip-chars-forward " \t")
			(delete-region (line-beginning-position) (point))
			(indent-to target-col))))))

  (dolist (hook '(typescript-ts-mode-hook tsx-ts-mode-hook js-ts-mode-hook))
	(add-hook hook
			  (lambda ()
				(local-set-key (kbd "RET") 'my-typescript-smart-newline))))

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

(provide 'init-core)
;;; init-core.el ends here
