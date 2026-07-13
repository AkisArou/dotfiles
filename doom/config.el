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
;; (setq doom-theme 'doom-dark+)
(setq doom-theme 'vscode-dark-plus)

(defconst akisarou/editor-bg "#18191A")

(defun akisarou/remap-command-line-bg-h ()
  (face-remap-add-relative 'default :background akisarou/editor-bg)
  (face-remap-add-relative 'fringe :background akisarou/editor-bg)
  (face-remap-add-relative 'minibuffer-prompt :background akisarou/editor-bg))

(defun akisarou/apply-vscode-theme-overrides-h ()
  (let ((bg akisarou/editor-bg)
        (subtle-bg "#1E2022")
        (line-number-fg "#4A4D50")
        (current-line-number-fg "#8A8D91"))
    (custom-set-faces!
      `(default :background ,bg)
      `(fringe :background ,bg)
      `(hl-line :background ,subtle-bg)
      `(line-number :foreground ,line-number-fg :background ,bg)
      `(line-number-current-line :foreground ,current-line-number-fg :background ,subtle-bg)
      `(minibuffer-prompt :background ,bg)
      `(vertical-border :background ,bg)
      `(mode-line :background ,bg)
      `(mode-line-inactive :background ,bg)
      `(doom-modeline :background ,bg)
      `(doom-modeline-inactive :background ,bg)
      `(doom-modeline-bar :background ,bg)
      `(doom-modeline-bar-inactive :background ,bg))))

(defun akisarou/apply-command-line-bg-h ()
  (dolist (name '(" *Minibuf-0*" " *Minibuf-1*"
                  " *Echo Area 0*" " *Echo Area 1*"))
    (when-let ((buffer (get-buffer name)))
      (with-current-buffer buffer
        (akisarou/remap-command-line-bg-h)))))

(add-hook 'doom-load-theme-hook #'akisarou/apply-vscode-theme-overrides-h)
(add-hook 'doom-load-theme-hook #'akisarou/apply-command-line-bg-h)
(add-hook 'window-setup-hook #'akisarou/apply-command-line-bg-h)
(add-hook 'minibuffer-inactive-mode-hook #'akisarou/remap-command-line-bg-h)
(akisarou/apply-vscode-theme-overrides-h)
(akisarou/apply-command-line-bg-h)

(add-hook! '+dashboard-mode-hook
  (face-remap-add-relative 'default :background akisarou/editor-bg)
  (face-remap-add-relative 'fringe :background akisarou/editor-bg))

;; Free C-c for Evil escape and minibuffer quit. Doom's default config uses C-c
;; as an alternate leader key in `general-override-mode-map', which takes
;; precedence over minibuffer-local bindings.
(setq doom-leader-alt-key "M-SPC"
      doom-localleader-alt-key "M-SPC m")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(use-package! grease
  :commands (grease-open grease-toggle grease-here)
  :init
  (setq grease-sort-method 'type
        grease-show-hidden nil
        grease-preview-window-width 0.4)
  (map! :leader
        :desc "Toggle Grease" "e" #'grease-toggle))

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
              (setq-local overriding-local-map map))
            (akisarou/remap-command-line-bg-h)
            (face-remap-add-relative 'vertico-current :background akisarou/editor-bg)))

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
    (kbd "C-e") #'corfu-insert)
  (require 'cape))

(after! corfu-auto
  ;; Trigger immediately after the first character so lsp-mode can cache the
  ;; broad candidate list before the input becomes a non-contiguous pattern.
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 1))

(after! corfu-popupinfo
  (setq corfu-popupinfo-delay '(0.2 . 0.1)))

(after! cape
  ;; Present semantic, buffer-word, and snippet candidates together, then let
  ;; Fussy/fzf rank the combined table.  Keep `cape-file' separate because
  ;; multi-step file completion is not compatible with `cape-capf-super'.
  (defalias 'akisarou/cape-dabbrev-yasnippet-capf
    (cape-capf-sort
     (cape-capf-super #'cape-dabbrev #'yasnippet-capf)))
  (defalias 'akisarou/cape-lsp-dabbrev-yasnippet-capf
    (cape-capf-sort
     (cape-capf-super #'lsp-completion-at-point
                      #'cape-dabbrev
                      #'yasnippet-capf)))

  (defun akisarou/merge-editor-capfs-h ()
    (when (derived-mode-p 'prog-mode 'text-mode 'conf-mode)
      (let* ((lsp-p (bound-and-true-p lsp-completion-mode))
             (merged (if lsp-p
                         #'akisarou/cape-lsp-dabbrev-yasnippet-capf
                       #'akisarou/cape-dabbrev-yasnippet-capf))
             (old-capfs '(lsp-completion-at-point
                          cape-dabbrev
                          yasnippet-capf
                          akisarou/cape-dabbrev-yasnippet-capf
                          akisarou/cape-lsp-dabbrev-yasnippet-capf))
             (remaining
              (seq-remove (lambda (capf) (memq capf old-capfs))
                          completion-at-point-functions)))
        (setq-local completion-at-point-functions
                    (if lsp-p
                        (cons merged remaining)
                      (append remaining (list merged)))))))

  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook
                  yas-minor-mode-hook lsp-completion-mode-hook))
    (add-hook hook #'akisarou/merge-editor-capfs-h 90))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (akisarou/merge-editor-capfs-h))))

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
      (user-error "No oxlint LSP workspace for this buffer"))))

(defconst akisarou/tsgo-auto-import-specifier-exclude-regex
  "^(assert|async_hooks|buffer|child_process|cluster|console|crypto|dgram|dns|domain|events|fs|fs/promises|http|http2|https|inspector|module|net|os|path|path/posix|perf_hooks|process|punycode|querystring|readline|repl|stream|string_decoder|timers|tls|trace_events|tty|url|util|v8|vm|worker_threads|zlib)$")

(after! lsp-javascript
  ;; Prefer each project's TypeScript 7 `tsc' (which contains tsgo), falling
  ;; back to the same global tsgo executable used by the Neovim config.
  (setq lsp-clients-tsgo-path
        (expand-file-name "~/.local/share/nvim/mason/bin/tsgo")
        lsp-clients-tsgo-args '("--lsp" "--stdio"))

  ;; `ts-ls' has a higher built-in priority than tsgo, so disable it to make
  ;; tsgo the primary JavaScript/TypeScript client.  oxfmt and oxlint remain
  ;; active as add-on clients.
  (add-to-list 'lsp-disabled-clients 'ts-ls)

  (defun akisarou/tsgo-command ()
    (cons (or (akisarou/project-local-bin "tsc")
              (lsp-package-path 'tsgo))
          lsp-clients-tsgo-args))

  (defun akisarou/tsgo-language-settings ()
    `(:format (:enable :json-false)
      :preferences
      (:includePackageJsonAutoImports "on"
       :importModuleSpecifier "non-relative"
       :importModuleSpecifierEnding "js"
       :useAliasesForRenames :json-false
       :autoImportSpecifierExcludeRegexes
       [,akisarou/tsgo-auto-import-specifier-exclude-regex])))

  (defun akisarou/tsgo-settings ()
    (let ((options (akisarou/tsgo-language-settings)))
      `(:typescript ,options :javascript ,options)))

  (defun akisarou/tsgo-initialized-h (workspace)
    (with-lsp-workspace workspace
      (lsp--set-configuration (akisarou/tsgo-settings)))
    (let ((caps (lsp--workspace-server-capabilities workspace)))
      (lsp:set-server-capabilities-document-formatting-provider? caps nil)
      (lsp:set-server-capabilities-document-range-formatting-provider? caps nil)))

  (defun akisarou/tsgo-filter-client-capabilities-a (capabilities)
    ;; lsp-mode currently serializes its empty inline-completion capability as
    ;; JSON null.  TypeScript 7 rejects that value, so omit this unsupported
    ;; capability for tsgo instead of preventing the whole LSP handshake.
    (when (and (bound-and-true-p lsp--cur-workspace)
               (eq (lsp--client-server-id
                    (lsp--workspace-client lsp--cur-workspace))
                   'tsgo))
      (when-let ((text-document (assq 'textDocument capabilities)))
        (setcdr text-document
                (assq-delete-all 'inlineCompletion (cdr text-document)))))
    capabilities)

  (advice-remove #'lsp--client-capabilities
                 #'akisarou/tsgo-filter-client-capabilities-a)
  (advice-add #'lsp--client-capabilities :filter-return
              #'akisarou/tsgo-filter-client-capabilities-a)

  ;; Replace lsp-mode's default tsgo registration because it neither resolves
  ;; a project-local TypeScript nor sends workspace configuration yet.
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'akisarou/tsgo-command)
    :activation-fn #'lsp-typescript-javascript-tsx-jsx-activate-p
    :priority -4
    :completion-in-comments? t
    :initialized-fn #'akisarou/tsgo-initialized-h
    :server-id 'tsgo
    :download-server-fn
    (lambda (_client callback error-callback _update?)
      (lsp-package-ensure 'tsgo callback error-callback)))))

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

(use-package! fussy
  ;; Doom's Vertico module configures Orderless on first input. Load Fussy
  ;; afterwards so fzf remains the final matching and ranking layer.
  :after orderless
  :config
  (fussy-setup-fzf)
  (fussy-corfu-setup)
  (setq completion-styles '(fussy basic)
        completion-category-defaults nil
        completion-category-overrides
        '((lsp-capf (styles fussy basic))
          (file (styles fussy basic partial-completion))
          (project-file (styles fussy basic))
          (buffer (styles fussy basic))
          (consult-location (styles fussy basic))
          (unicode-name (styles fussy basic))
          (xref-location (styles fussy basic))
          (info-menu (styles fussy basic))
          (symbol-help (styles fussy basic)))))

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
