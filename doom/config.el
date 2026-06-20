;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dark+)

;; Free C-c for Evil escape and minibuffer quit. Doom's default config uses C-c
;; as an alternate leader key in `general-override-mode-map', which takes
;; precedence over minibuffer-local bindings.
(setq doom-leader-alt-key "M-SPC"
      doom-localleader-alt-key "M-SPC m")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Keep point centered while scrolling, matching the behavior from the old config.
(setq scroll-preserve-screen-position t
      scroll-conservatively 0
      maximum-scroll-margin 0.5
      scroll-margin 99999)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(add-hook 'minibuffer-setup-hook
          (lambda ()
            (let ((map (copy-keymap (current-local-map))))
              (define-key map (kbd "C-c") #'abort-minibuffers)
              (setq-local overriding-local-map map))))

(defun akisarou/vertico-backward-kill-word ()
  (interactive)
  (if (bound-and-true-p evil-local-mode)
      (evil-delete-backward-word 1)
    (backward-kill-word 1)))

(after! vertico
  (define-key vertico-map (kbd "C-w") #'akisarou/vertico-backward-kill-word)
  (define-key vertico-map (kbd "C-e") #'vertico-exit)
  (define-key vertico-map (kbd "C-d") #'vertico-scroll-up)
  (define-key vertico-map (kbd "C-u") #'vertico-scroll-down))

(after! evil-escape
  (evil-define-key* '(insert replace visual operator) 'global
    (kbd "C-c") #'evil-escape))

(after! evil
  (setq evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'box
        evil-replace-state-cursor 'hbar
        evil-motion-state-cursor 'box
        evil-emacs-state-cursor 'hbar)

  (defun akisarou/terminal-cursor-update-h ()
    (unless (display-graphic-p)
      (send-string-to-terminal
       (cond
        ((evil-insert-state-p) "\e[6 q")
        ((evil-emacs-state-p) "\e[5 q")
        (t "\e[2 q")))))

  (add-hook 'post-command-hook #'akisarou/terminal-cursor-update-h))

(after! corfu
  (setq corfu-preselect 'first
        corfu-cycle t
        corfu-quit-no-match nil
        corfu-quit-at-boundary nil)
  (define-key corfu-map (kbd "C-e") #'corfu-insert)
  (evil-define-key* '(insert replace) corfu-map
    (kbd "C-e") #'corfu-insert))

(after! corfu-auto
  ;; Trigger immediately after the first character so lsp-mode can cache the
  ;; broad candidate list before the input becomes a non-contiguous pattern.
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 1))

(after! corfu-popupinfo
  (setq corfu-popupinfo-delay '(0.2 . 0.1)))

(after! lsp-completion
  ;; Let lsp-mode reuse broad server results and leave fuzzy narrowing to the
  ;; completion style instead of over-filtering incomplete server responses.
  (setq lsp-completion-no-cache nil
        lsp-completion-use-last-result t
        lsp-completion-filter-on-incomplete nil
        lsp-completion-sort-initial-results nil))

(after! lsp-mode
  ;; Compatibility with tsgo's strict JSON decoder:
  ;; https://github.com/emacs-lsp/lsp-mode/issues/5081
  (defun akisarou/lsp-tsgo-capabilities-a (capabilities)
    (when-let* ((text-document (alist-get 'textDocument capabilities))
                (inline-completion (assq 'inlineCompletion text-document)))
      (setcdr inline-completion (make-hash-table :test 'equal)))
    capabilities)

  (defun akisarou/lsp-omit-nil-params-a (message)
    (if (plist-get message :params)
        message
      (let ((copy (copy-sequence message)))
        (cl-remf copy :params)
        copy)))

  (defun akisarou/lsp-omit-nil-initialization-options-a (params)
    (if (plist-get params :initializationOptions)
        params
      (let ((copy (copy-sequence params)))
        (cl-remf copy :initializationOptions)
        copy)))

  (defun akisarou/lsp-omit-nil-request-params-a (args)
    (pcase-let ((`(,body . ,rest) args))
      (if (plist-get body :params)
          args
        (let ((copy (copy-sequence body)))
          (cl-remf copy :params)
          `(,copy ,@rest)))))

  (defun akisarou/lsp-omit-nil-initialize-options-args-a (args)
    (pcase-let ((`(,method ,params . ,rest) args))
      (if (and (equal method "initialize")
               (not (plist-get params :initializationOptions)))
          `(,method ,(akisarou/lsp-omit-nil-initialization-options-a params) ,@rest)
        args)))

  (advice-add #'lsp--client-capabilities :filter-return
              #'akisarou/lsp-tsgo-capabilities-a)
  (advice-add #'lsp--make-notification :filter-return
              #'akisarou/lsp-omit-nil-params-a)
  (advice-add #'lsp--send-request-async :filter-args
              #'akisarou/lsp-omit-nil-request-params-a)
  (advice-add #'lsp-request-async :filter-args
              #'akisarou/lsp-omit-nil-initialize-options-args-a))

(after! orderless
  ;; Make Vertico project-file matching fuzzy enough for inputs like
  ;; "packalua" to match "nvim/lua/config/packages.lua".
  (setq orderless-matching-styles '(orderless-flex))
  (setq completion-category-overrides
        '((lsp-capf (styles orderless basic))
          (file (styles orderless partial-completion))
          (project-file (styles orderless)))))

(use-package! codex
  :config
  (define-key doom-leader-map (kbd "a") nil)
  (map! :leader
        (:prefix ("a" . "ai")
         :desc "Codex" "a" codex-command-map)))


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `with-eval-after-load' block, otherwise Doom's defaults may override your
;; settings. E.g.
;;
;;   (with-eval-after-load 'PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look them up).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
