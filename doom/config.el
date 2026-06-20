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

;; Match common editor behavior: C-s saves the current buffer.
(map! "C-s" #'save-buffer)

(defun akisarou/save-buffers-kill-emacs-no-prompt ()
  (interactive)
  (save-some-buffers t)
  (kill-emacs))

(setq confirm-kill-emacs nil
      confirm-kill-processes nil)

(global-set-key [remap save-buffers-kill-emacs]
                #'akisarou/save-buffers-kill-emacs-no-prompt)
(global-set-key [remap save-buffers-kill-terminal]
                #'akisarou/save-buffers-kill-emacs-no-prompt)

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

(defconst akisarou/oxfmt-languages
  '("javascript" "javascriptreact" "typescript" "typescriptreact"
    "toml" "json" "jsonc" "json5" "yaml" "html" "vue" "handlebars"
    "css" "scss" "less" "graphql" "markdown"))

(defconst akisarou/oxfmt-modes
  '(js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode
    json-mode json-ts-mode jsonc-mode yaml-mode yaml-ts-mode html-mode
    web-mode css-mode css-ts-mode scss-mode less-css-mode graphql-mode
    markdown-mode markdown-ts-mode gfm-mode conf-toml-mode toml-ts-mode))

(defconst akisarou/oxlint-languages
  '("javascript" "javascriptreact" "typescript" "typescriptreact"))

(after! lsp-mode
  (setq lsp-enable-file-watchers nil)

  (defun akisarou/project-local-bin (name)
    (when-let* ((root (or (when (fboundp 'doom-project-root)
                            (doom-project-root))
                          (locate-dominating-file default-directory "package.json")
                          (locate-dominating-file default-directory ".git")))
                (bin (expand-file-name (format "node_modules/.bin/%s" name) root)))
      (when (file-executable-p bin)
        bin)))

  (defun akisarou/oxfmt-command ()
    (list (or (akisarou/project-local-bin "oxfmt") "oxfmt") "--lsp"))

  (defun akisarou/oxlint-command ()
    (list (akisarou/project-local-bin "oxlint") "--lsp"))

  (defun akisarou/oxlint-available-p ()
    (and (akisarou/project-local-bin "oxlint") t))

  (defun akisarou/lsp-activate-on-oxfmt-languages (file-name mode)
    (and (buffer-file-name)
         (funcall (apply #'lsp-activate-on akisarou/oxfmt-languages)
                  file-name mode)))

  (defun akisarou/lsp-activate-on-oxlint-languages (file-name mode)
    (and (akisarou/oxlint-available-p)
         (funcall (apply #'lsp-activate-on akisarou/oxlint-languages)
                  file-name mode)))

  (add-to-list 'lsp-language-id-configuration '("\\.json5\\'" . "json5"))
  (add-to-list 'lsp-language-id-configuration '("\\.hbs\\'" . "handlebars"))
  (add-to-list 'lsp-language-id-configuration '("\\.handlebars\\'" . "handlebars"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'akisarou/oxfmt-command)
    :activation-fn #'akisarou/lsp-activate-on-oxfmt-languages
    :priority -1
    :add-on? t
    :server-id 'oxfmt))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'akisarou/oxlint-command
                                           #'akisarou/oxlint-available-p)
    :activation-fn #'akisarou/lsp-activate-on-oxlint-languages
    :priority -1
    :add-on? t
    :server-id 'oxlint))

  (defun akisarou/oxlint-fix-all ()
    (interactive)
    (unless (buffer-file-name)
      (user-error "Current buffer is not visiting a file"))
    (if-let ((workspace (lsp-find-workspace 'oxlint (buffer-file-name))))
        (with-lsp-workspace workspace
          (lsp-request "workspace/executeCommand"
                       `(:command "oxc.fixAll"
                         :arguments [(:uri ,(lsp--buffer-uri))])))
      (user-error "No oxlint LSP workspace for this buffer")))

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

(cl-defun akisarou/format-with-oxfmt-lsp (&key buffer scratch callback &allow-other-keys)
  (with-current-buffer buffer
    (if-let ((workspace (and buffer-file-name
                             (lsp-find-workspace 'oxfmt buffer-file-name))))
        (with-lsp-workspace workspace
          (if (lsp-feature? "textDocument/formatting")
              (let ((edits (lsp-request "textDocument/formatting"
                                        (lsp--make-document-formatting-params))))
                (unless (seq-empty-p edits)
                  (with-current-buffer scratch
                    (lsp--apply-text-edits edits 'format)))
                (funcall callback))
            (funcall callback "oxfmt LSP does not support document formatting")))
      (funcall callback "oxfmt LSP is not running for this buffer"))))

(set-formatter! 'oxfmt-lsp #'akisarou/format-with-oxfmt-lsp
  :modes akisarou/oxfmt-modes)

(add-hook! '(markdown-mode-local-vars-hook
             markdown-ts-mode-local-vars-hook
             gfm-mode-local-vars-hook
             conf-toml-mode-local-vars-hook
             toml-ts-mode-local-vars-hook
             graphql-mode-local-vars-hook)
           :append #'lsp!)

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
