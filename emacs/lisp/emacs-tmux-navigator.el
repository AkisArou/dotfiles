;;; emacs-tmux-navigator.el --- Navigate between Emacs windows and tmux panes seamlessly -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.4") (evil "1.0.0"))
;; Keywords: tmux, navigation, convenience, evil, vim
;; URL: https://github.com/yourusername/emacs-tmux-navigator

;;; Commentary:

;; This package provides seamless navigation between Emacs windows and tmux panes
;; using vim-style navigation (C-h, C-j, C-k, C-l) specifically designed for Evil mode.
;;
;; When you navigate in a direction where there's no Emacs window, the command
;; is forwarded to tmux to switch panes.
;;
;; Usage:
;;   (require 'emacs-tmux-navigator)
;;   (emacs-tmux-navigator-mode 1)
;;
;; Or with use-package:
;;   (use-package emacs-tmux-navigator
;;     :after evil
;;     :config
;;     (emacs-tmux-navigator-mode 1))

;;; Code:

(require 'evil nil t)
(require 'windmove)

(defgroup emacs-tmux-navigator nil
  "Navigate between Emacs windows and tmux panes."
  :group 'convenience
  :prefix "emacs-tmux-navigator-")

(defcustom emacs-tmux-navigator-use-internal-api t
  "Use Emacs internal window navigation when possible.
When nil, always call tmux to check if navigation is possible."
  :type 'boolean
  :group 'emacs-tmux-navigator)

(defun emacs-tmux-navigator--in-tmux-p ()
  "Return non-nil if running inside a tmux session."
  (and (getenv "TMUX")
       (executable-find "tmux")))

(defun emacs-tmux-navigator--tmux-command (direction)
  "Send tmux command to select pane in DIRECTION.
DIRECTION should be one of: L (left), D (down), U (up), R (right)."
  (let ((cmd (format "tmux select-pane -%s" direction)))
    (shell-command cmd)))

(defun emacs-tmux-navigator--window-exists-p (direction)
  "Check if there's an Emacs window in DIRECTION.
DIRECTION should be 'left, 'right, 'up, or 'down."
  (let ((window (windmove-find-other-window direction)))
    (and window (not (eq window (selected-window))))))

(defun emacs-tmux-navigator--navigate (direction tmux-direction)
  "Navigate in DIRECTION, falling back to tmux with TMUX-DIRECTION if needed.
DIRECTION is a symbol: 'left, 'right, 'up, or 'down.
TMUX-DIRECTION is a string: L, R, U, or D."
  (if (emacs-tmux-navigator--window-exists-p direction)
      (windmove-do-window-select direction)
    ;; No Emacs window in this direction
    (if (emacs-tmux-navigator--in-tmux-p)
        (emacs-tmux-navigator--tmux-command tmux-direction)
      ;; Not in tmux, do nothing or beep
      (message "No window in this direction"))))

(defun emacs-tmux-navigator-left ()
  "Navigate left to an Emacs window or tmux pane."
  (interactive)
  (emacs-tmux-navigator--navigate 'left "L"))

(defun emacs-tmux-navigator-down ()
  "Navigate down to an Emacs window or tmux pane."
  (interactive)
  (emacs-tmux-navigator--navigate 'down "D"))

(defun emacs-tmux-navigator-up ()
  "Navigate up to an Emacs window or tmux pane."
  (interactive)
  (emacs-tmux-navigator--navigate 'up "U"))

(defun emacs-tmux-navigator-right ()
  "Navigate right to an Emacs window or tmux pane."
  (interactive)
  (emacs-tmux-navigator--navigate 'right "R"))

;; Evil mode integration
(defun emacs-tmux-navigator--setup-evil-bindings ()
  "Set up Evil mode keybindings for tmux navigation."
  (when (featurep 'evil)
    ;; Define keys in normal, visual, and motion states
    (evil-define-key* '(normal visual motion) 'global
      (kbd "C-h") #'emacs-tmux-navigator-left
      (kbd "C-j") #'emacs-tmux-navigator-down
      (kbd "C-k") #'emacs-tmux-navigator-up
      (kbd "C-l") #'emacs-tmux-navigator-right)
    
    ;; Also bind in insert and emacs states for consistency
    (evil-define-key* '(insert emacs) 'global
      (kbd "C-h") #'emacs-tmux-navigator-left
      (kbd "C-j") #'emacs-tmux-navigator-down
      (kbd "C-k") #'emacs-tmux-navigator-up
      (kbd "C-l") #'emacs-tmux-navigator-right)))

(defun emacs-tmux-navigator--remove-evil-bindings ()
  "Remove Evil mode keybindings for tmux navigation."
  (when (featurep 'evil)
    ;; Restore default bindings
    (evil-define-key* '(normal visual motion) 'global
      (kbd "C-h") nil
      (kbd "C-j") nil
      (kbd "C-k") nil
      (kbd "C-l") nil)
    
    (evil-define-key* '(insert emacs) 'global
      (kbd "C-h") nil
      (kbd "C-j") nil
      (kbd "C-k") nil
      (kbd "C-l") nil)))

;; Regular keymap (fallback for non-Evil usage)
(defvar emacs-tmux-navigator-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-h") #'emacs-tmux-navigator-left)
    (define-key map (kbd "C-j") #'emacs-tmux-navigator-down)
    (define-key map (kbd "C-k") #'emacs-tmux-navigator-up)
    (define-key map (kbd "C-l") #'emacs-tmux-navigator-right)
    map)
  "Keymap for `emacs-tmux-navigator-mode'.")

;;;###autoload
(define-minor-mode emacs-tmux-navigator-mode
  "Minor mode for seamless navigation between Emacs windows and tmux panes.
Integrates with Evil mode when available."
  :global t
  :keymap emacs-tmux-navigator-mode-map
  :group 'emacs-tmux-navigator
  (if emacs-tmux-navigator-mode
      (progn
        ;; Set up Evil bindings if Evil is loaded
        (if (featurep 'evil)
            (emacs-tmux-navigator--setup-evil-bindings)
          ;; If Evil isn't loaded yet, set up a hook
          (add-hook 'evil-mode-hook #'emacs-tmux-navigator--setup-evil-bindings))
        (message "Emacs-tmux-navigator mode enabled"))
    (progn
      (emacs-tmux-navigator--remove-evil-bindings)
      (remove-hook 'evil-mode-hook #'emacs-tmux-navigator--setup-evil-bindings)
      (message "Emacs-tmux-navigator mode disabled"))))

;; Ensure bindings are set up when Evil loads after this package
(with-eval-after-load 'evil
  (when emacs-tmux-navigator-mode
    (emacs-tmux-navigator--setup-evil-bindings)))

(provide 'emacs-tmux-navigator)

;;; emacs-tmux-navigator.el ends here
