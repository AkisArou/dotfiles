(setq doom-theme 'doom-dracula)

(setq display-line-numbers-type t)

(setq org-directory "~/org/")

(setq confirm-kill-emacs nil)

(define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "C-c") 'evil-normal-state)

(map! :nv "C-h" #'evil-window-left
      :nv "C-l" #'evil-window-right
      :n  "C-j" #'evil-window-down
      :n  "C-k" #'evil-window-up

      :n "SPC b a" #'doom/kill-all-buffers
      :n "SPC b o" #'doom/kill-other-buffers

      :n "SPC e" #'+treemacs/toggle

      :n "g r" #'+lookup/references
      :n "K"   #'lsp-describe-thing-at-point

      :nv "H" #'centaur-tabs-backward
      :nv "L" #'centaur-tabs-forward
      )


;;
;; auto-save
;;
(super-save-mode +1)

(setq auto-save-default nil)

(after! super-save
  (setq super-save-auto-save-when-idle t))


;;
;; lsp
;;

(use-package! lsp-biome
  :init
  (setq lsp-biome-organize-imports-on-save t)
  (setq lsp-biome-organize-imports-on-save t))

(use-package! lsp-tailwindcss)
(add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save)

(after! lsp-mode
  (when (modulep! :completion corfu)
    (setq lsp-completion-provider :none)
    (add-hook 'lsp-mode-hook #'lsp-completion-mode)))


;;
;; treemacs
;;

(after! treemacs
  (evil-define-key 'treemacs treemacs-mode-map (kbd "C-l") #'evil-window-right)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "SPC e") #'+treemacs/toggle)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "W") #'treemacs-collapse-project)
  (evil-define-key 'treemacs treemacs-mode-map (kbd "H") #'treemacs-toggle-show-dotfiles))

;;
;; orderless
;;

(with-eval-after-load 'orderless
  (setq completion-styles '(orderless flex))
  )

;;
;; corfu
;;

(with-eval-after-load 'corfu
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))

  (setq corfu-auto t)               ;; Enable auto completion
  (setq corfu-preselect 'directory) ;; Select the first candidate, except for directories

  (map! :map corfu-map
        :i "TAB" #'corfu-insert
        :i "tab" #'corfu-insert
        :i "backspace" nil
        )
  )

(global-corfu-mode 1)
