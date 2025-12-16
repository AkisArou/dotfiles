;;; init-completion.el --- Completion Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for completion

;;; Code:
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


;;; CORFU
(use-package corfu
  :ensure t
  :straight t
  :defer t
  :custom
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
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

  (defun my/corfu-popupinfo-skip-tailwind (orig-fun &rest args)
	"Skip popupinfo for Module type completions (kind 9)."
	(let* ((candidate (and (>= corfu--index 0)
						   (< corfu--index (length corfu--candidates))
						   (nth corfu--index corfu--candidates)))
		   (props (and candidate (text-properties-at 0 candidate)))
		   (lsp-item (plist-get props 'lsp-completion-unresolved-item))
		   (kind (and lsp-item (plist-get lsp-item :kind))))
	  ;; Skip if it's kind 9 (Module)
	  (unless (eq kind 9)
		(apply orig-fun args))))

  (advice-add 'corfu-popupinfo--show :around #'my/corfu-popupinfo-skip-tailwind)


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
(use-package nerd-icons-corfu
  :ensure t
  :straight t
  :after (:all corfu))


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



;;; ELDOC-BOX
;; eldoc-box enhances the default Eldoc experience by displaying documentation in a popup box,
;; usually in a child frame. This makes it easier to read longer docstrings without relying on
;; the minibuffer. It integrates seamlessly with Eldoc and activates when Eldoc is active.
;; Useful for graphical Emacs; terminal users may want to fall back to `eldoc-box-display-at-point-mode'.
(use-package eldoc-box
  :ensure t
  :straight t
  :defer t)

(provide 'init-completion)
;;; init-completion.el ends here
