;;; init-ui.el --- UI Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; UI Configuration

;;; Code:

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
  (setq doom-modeline-icon t)
  :hook
  (after-init . doom-modeline-mode))

(with-eval-after-load 'doom-modeline
  (setq ring-bell-function #'ignore))

;;; NERD ICONS
;; The `nerd-icons' package provides a set of icons for use in Emacs. These icons can
;; enhance the visual appearance of various modes and packages, making it easier to
;; distinguish between different file types and functionalities.
(use-package nerd-icons
  :ensure t                               ;; Ensure the package is installed.
  :straight t
  :defer t)                               ;; Load the package only when needed to improve startup time.


;;; NERD ICONS Dired
;; The `nerd-icons-dired' package integrates nerd icons into the Dired mode,
;; providing visual icons for files and directories. This enhances the Dired
;; interface by making it easier to identify file types at a glance.
(use-package nerd-icons-dired
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
   `(evil-ex-search ((t (:foreground "#16161e" :background "#bb9af7"))))
   `(lazy-highlight ((t (:background "#5e428f"))))
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



;;; shr
(setq shr-use-colors nil)



(provide 'init-ui)
;;; init-ui.el ends here
