;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Appearance

(setq doom-theme 'vscode-dark-plus)

(defconst akisarou/editor-bg "#18191A")

(defface akisarou-control-flow-face '((t :inherit font-lock-keyword-face))
  "Control-flow keywords, separate from declarations and type keywords."
  :group 'font-lock-faces)
(defface akisarou-builtin-type-face '((t :inherit font-lock-type-face))
  "Primitive C types, separate from user-defined types."
  :group 'font-lock-faces)
(defface akisarou-jsx-delimiter-face '((t :inherit font-lock-bracket-face))
  "JSX tag delimiters, separate from expression brackets."
  :group 'font-lock-faces)

(defun akisarou/remap-command-line-bg-h ()
  (face-remap-add-relative 'default :background akisarou/editor-bg)
  (face-remap-add-relative 'fringe :background akisarou/editor-bg)
  (face-remap-add-relative 'minibuffer-prompt :background akisarou/editor-bg))

(defun akisarou/apply-vscode-theme-overrides-h ()
  (let ((bg akisarou/editor-bg)
        (fg "#AEAFAD")
        (orange "#C48081")
        (subtle-bg "#222222")
        (line-number-fg "#5A5A5A")
        (current-line-number-fg "#BBBBBB"))
    (custom-set-faces!
      `(default :background ,bg :foreground "#D4D4D4")
      `(fringe :background ,bg)
      `(hl-line :background ,subtle-bg)
      '(cursor :background "#AEAFAD")
      ;; vscode.nvim: map semantic groups as well as the shared palette.
      '(font-lock-comment-face :foreground "#6A9955" :slant normal :weight normal)
      '(font-lock-doc-face :inherit font-lock-comment-face :foreground "#6A9955")
      '(font-lock-keyword-face :foreground "#569CD6" :weight normal)
      '(font-lock-preprocessor-face :foreground "#C586C0")
      '(akisarou-control-flow-face :foreground "#C586C0")
      '(font-lock-type-face :foreground "#4EC9B0")
      '(akisarou-builtin-type-face :foreground "#569CD6")
      '(font-lock-function-name-face :foreground "#DCDCAA" :weight normal)
      '(font-lock-function-call-face :inherit font-lock-function-name-face)
      '(font-lock-builtin-face :foreground "#DCDCAA")
      '(font-lock-variable-name-face :foreground "#9CDCFE")
      '(font-lock-variable-use-face :inherit font-lock-variable-name-face)
      '(font-lock-property-name-face :inherit font-lock-variable-name-face)
      '(font-lock-property-use-face :inherit font-lock-variable-name-face)
      '(font-lock-constant-face :foreground "#4FC1FF")
      '(font-lock-number-face :foreground "#B5CEA8")
      '(font-lock-escape-face :foreground "#D4D4D4")
      '(font-lock-operator-face :foreground "#D4D4D4")
      '(font-lock-negation-char-face :foreground "#D4D4D4")
      '(font-lock-punctuation-face :foreground "#D4D4D4")
      '(typescript-ts-jsx-tag-face :foreground "#569CD6")
      '(typescript-ts-jsx-attribute-face :foreground "#9CDCFE")
      '(akisarou-jsx-delimiter-face :foreground "#808080")
      '(lsp-face-semhl-macro :inherit font-lock-constant-face)
      '(lsp-face-semhl-operator :inherit font-lock-operator-face)
      '(lsp-face-semhl-number :inherit font-lock-number-face)
      '(lsp-face-semhl-namespace :inherit font-lock-type-face :weight normal)
      ;; clangd marks inactive preprocessor branches as semantic comments.
      '(lsp-face-semhl-comment :foreground "#51504F")
      '(hl-todo :foreground "#18191A" :weight bold :inverse-video nil)
      ;; colorscheme.lua: vscOrange, Visual, Search, IncSearch, CurSearch.
      `(font-lock-string-face :foreground ,orange)
      `(web-mode-string-face :foreground ,orange)
      `(web-mode-html-attr-value-face :foreground ,orange)
      '(region :background "#313131" :foreground unspecified)
      '(lazy-highlight :background "#2a4856" :foreground unspecified)
      '(evil-ex-lazy-highlight :background "#2a4856" :foreground unspecified)
      '(isearch :background "#444444" :foreground unspecified)
      '(evil-ex-search :background "#17262d" :foreground unspecified)
      ;; Snippet fields and symbol occurrences use the same muted teal palette.
      '(yas-field-highlight-face :background "#17262d")
      '(match :background "#1a2d38" :foreground unspecified)
      '(lsp-face-highlight-textual :background "#1a2d38" :underline nil)
      '(lsp-face-highlight-read :background "#1a2d38" :underline nil)
      '(lsp-face-highlight-write :background "#1a2d38" :underline nil)
      '(highlight-thing :background "#17262d")
      ;; Match Neovim's delimiter highlight while preserving syntax foregrounds.
      '(show-paren-match :background "#343B41" :foreground unspecified
        :weight normal :extend nil)
      '(show-paren-match-expression :background "#343B41" :foreground unspecified
        :weight normal :extend nil)
      ;; Oil uses ordinary file text and blue directory names.
      '(grease-file-face :inherit default :foreground unspecified)
      '(grease-directory-face :inherit default :foreground "#569cd6")
      '(grease-hidden-face :inherit shadow :foreground unspecified)
      '(akisarou/grease-ignored-face :foreground "#6a9955")
      '(akisarou/grease-added-face :foreground "#81b88b")
      '(akisarou/grease-modified-face :foreground "#e2c08d")
      '(akisarou/grease-deleted-face :foreground "#E4676B")
      ;; PmenuSel and the fzf picker border/header colors.
      '(corfu-default :foreground "#D4D4D4" :background "#202020")
      '(corfu-annotations :foreground "#BBBBBB")
      '(corfu-current :background "#343B41" :foreground unspecified)
      '(vertico-current :inherit nil :background unspecified :foreground "#569cd6"
        :weight bold :underline nil)
      '(corfu-border :background "#444444" :foreground "#444444")
      '(vertico-group-title :foreground "#343B41")
      '(consult-key :foreground "#343B41")
      ;; Keep diagnostic text and the underline separate, like Neovim's faces.
      `(error :foreground "#E4676B" :background ,bg)
      `(warning :foreground "#DCDCAA" :background ,bg)
      '(success :foreground "#4EC9B0")
      `(flycheck-fringe-error :foreground "#E4676B" :background ,bg)
      `(flycheck-fringe-warning :foreground "#DCDCAA" :background ,bg)
      `(flycheck-fringe-info :foreground "#569CD6" :background ,bg)
      `(flycheck-error-list-error :foreground "#E4676B" :background ,bg)
      '(flycheck-error :foreground unspecified :background unspecified
        :underline (:style wave :color "#A0484B"))
      '(flymake-error :foreground unspecified :background unspecified
        :underline (:style wave :color "#A0484B"))
      '(diff-added :foreground unspecified :background "#4B5632")
      '(diff-removed :foreground unspecified :background "#6F1313")
      '(diff-changed :foreground unspecified :background "#4B1818")
      '(diff-hl-insert :foreground "#6A9955" :background unspecified)
      '(diff-hl-change :foreground "#DCDCAA" :background unspecified)
      '(diff-hl-delete :foreground "#F44747" :background unspecified)
      `(line-number :foreground ,line-number-fg :background ,bg)
      `(line-number-current-line :foreground ,current-line-number-fg :background ,bg)
      `(minibuffer-prompt :background ,bg)
      `(vertical-border :foreground "#444444" :background ,bg)
      '(window-divider :foreground "#444444")
      '(window-divider-first-pixel :foreground "#444444")
      '(window-divider-last-pixel :foreground "#444444")
      ;; lualine's active and inactive sections share this background and text.
      `(mode-line :foreground ,fg :background ,bg)
      `(mode-line-inactive :foreground ,fg :background ,bg)
      `(doom-modeline :foreground ,fg :background ,bg)
      `(doom-modeline-inactive :foreground ,fg :background ,bg)
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

(defun akisarou/vscode-ts-font-lock-h ()
  "Match vscode.nvim's syntax categories using buffer-local parser rules."
  (let ((language (pcase major-mode
                    ('c-ts-mode 'c) ('c++-ts-mode 'cpp)
                    ('js-ts-mode 'javascript)
                    ('typescript-ts-mode 'typescript) ('tsx-ts-mode 'tsx))))
    (when language
      (treesit-add-font-lock-rules
       (treesit-font-lock-rules
        :language language :feature 'keyword :override t
        (if (memq language '(c cpp))
            '(["if" "else" "case" "switch" "while" "for" "do"
               "continue" "break" "return"] @akisarou-control-flow-face)
          '(["if" "else" "switch" "case" "default" "while" "for" "do"
             "break" "continue" "return" "yield" "throw" "try" "catch"
             "finally" "import" "export" "from"] @akisarou-control-flow-face)))
       :after)
      (when (memq language '(c cpp))
        ;; Neovim uses clangd's semantic tokens for macros and inactive code.
        ;; Declaration/definition modifiers should not change syntax colors.
        (setq-local lsp-semantic-tokens-enable t
                    lsp-semantic-token-modifier-faces
                    '(("readonly" . lsp-face-semhl-constant)
                      ("deprecated" . lsp-face-semhl-deprecated)
                      ("controlFlow" . akisarou-control-flow-face)))
        (treesit-add-font-lock-rules
         (treesit-font-lock-rules
          :language language :feature 'type :override t
          '((primitive_type) @akisarou-builtin-type-face
            ["long" "short" "signed" "unsigned"] @akisarou-builtin-type-face)
          :language language :feature 'constant :override t
          '([(true) (false) (null)] @font-lock-keyword-face
            ((identifier) @font-lock-constant-face
             (:match "\\`[A-Z][A-Z0-9_]+\\'" @font-lock-constant-face)))
          :language language :feature 'string :override t
          '((char_literal) @font-lock-string-face))
         :after))
      (when (memq language '(javascript typescript tsx))
        (treesit-add-font-lock-rules
         (treesit-font-lock-rules
          :language language :feature 'constant :override t
          '([(true) (false) (null) (undefined)] @font-lock-keyword-face)
          :language language :feature 'variable
          '((identifier) @font-lock-variable-use-face
            (property_identifier) @font-lock-property-use-face))
         :after))
      (when (eq language 'tsx)
        ;; vscode.nvim's after/queries/jsx/highlights.scm treats component
        ;; names (including namespaced/custom elements) as types.
        (treesit-add-font-lock-rules
         (treesit-font-lock-rules
          :language 'tsx :feature 'jsx :override t
          '((jsx_opening_element ["<" ">"] @akisarou-jsx-delimiter-face)
            (jsx_closing_element ["</" ">"] @akisarou-jsx-delimiter-face)
            (jsx_self_closing_element ["<" "/>"] @akisarou-jsx-delimiter-face)
            (jsx_opening_element
             name: (identifier) @font-lock-type-face
             (:match "[A-Z-]" @font-lock-type-face))
            (jsx_closing_element
             name: (identifier) @font-lock-type-face
             (:match "[A-Z-]" @font-lock-type-face))
            (jsx_self_closing_element
             name: (identifier) @font-lock-type-face
             (:match "[A-Z-]" @font-lock-type-face))
            (jsx_opening_element
             name: (member_expression
                    object: (identifier) @font-lock-type-face
                    property: (property_identifier) @font-lock-type-face))
            (jsx_closing_element
             name: (member_expression
                    object: (identifier) @font-lock-type-face
                    property: (property_identifier) @font-lock-type-face))
            (jsx_self_closing_element
             name: (member_expression
                    object: (identifier) @font-lock-type-face
                    property: (property_identifier) @font-lock-type-face))))
         :after)))))

(dolist (hook '(c-ts-mode-hook c++-ts-mode-hook js-ts-mode-hook
                typescript-ts-mode-hook tsx-ts-mode-hook))
  (add-hook hook #'akisarou/vscode-ts-font-lock-h))

(after! hl-todo
  ;; todo-comments.nvim: colored keyword backgrounds and matching body text.
  (setq hl-todo-color-background t
        hl-todo-require-punctuation t
        hl-todo-keyword-faces
        '(("FIX\\|FIXME\\|BUG\\|FIXIT\\|ISSUE" . "#E4676B")
          ("TODO\\|NOTE\\|INFO" . "#569CD6")
          ("HACK\\|WARN\\|WARNING\\|XXX" . "#DCDCAA")
          ("PERF\\|OPTIM\\|PERFORMANCE\\|OPTIMIZE\\|TEST\\|TESTING\\|PASSED\\|FAILED"
           . "#9CDCFE")))

  (defvar-local akisarou/todo-body-color nil)
  (defun akisarou/match-todo-body (limit)
    "Find a todo comment body up to LIMIT using the configured keyword colors."
    (catch 'found
      (while (re-search-forward
              (concat "\\_<\\(" (mapconcat #'car hl-todo-keyword-faces "\\|")
                      "\\)\\_>[ \t]*:") limit t)
        (let* ((keyword (match-string-no-properties 1))
               (start (point))
               (comment-start (save-excursion
                                (let ((state (syntax-ppss (match-beginning 1))))
                                  (and (nth 4 state) (nth 8 state))))))
          (when comment-start
            (setq akisarou/todo-body-color
                  (cdr (seq-find (lambda (entry)
                                   (string-match-p (concat "\\`\\(?:" (car entry) "\\)\\'")
                                                   keyword))
                                 hl-todo-keyword-faces)))
            (let ((end (min limit (line-end-position)
                            (save-excursion
                              (goto-char comment-start)
                              (forward-comment 1)
                              (point)))))
              (when (< start end)
                (set-match-data (list start end))
                (goto-char end)
                (throw 'found t))))))))

  (defun akisarou/todo-comment-style-h ()
    (let ((keywords '((akisarou/match-todo-body
                       (0 (list :foreground akisarou/todo-body-color) t)))))
      (if hl-todo-mode
          (font-lock-add-keywords nil keywords t)
        (font-lock-remove-keywords nil keywords))))
  (add-hook 'hl-todo-mode-hook #'akisarou/todo-comment-style-h))

;; Free C-c for Evil escape and minibuffer quit. Doom's default config uses C-c
;; as an alternate leader key in `general-override-mode-map', which takes
;; precedence over minibuffer-local bindings.
(setq doom-leader-alt-key "M-SPC"
      doom-localleader-alt-key "M-SPC m")

(setq display-line-numbers-type 'relative)

(setq-default indent-tabs-mode nil
              tab-width 2)

;; Disable automatic whitespace markers; +guess and +trim remain independent.
(remove-hook 'after-change-major-mode-hook
             #'+whitespace-highlight-incorrect-indentation-h)
(remove-hook 'editorconfig-after-apply-functions
             #'+whitespace-highlight-incorrect-indentation-again-h)
(after! whitespace
  (global-whitespace-mode -1)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p whitespace-mode)
        (whitespace-mode -1)))))

;;; Syntax parsers

;; Doom supplies versioned grammar recipes and installs into its profile data
;; directory. Native/registered modes install missing dependencies on first use.
(after! treesit
  (setq treesit-auto-install-grammar 'always))

;; These native modes have no corresponding +tree-sitter flag in Doom.
;; bash-ts-mode delegates Zsh and other non-Bash dialects back to sh-mode.
(set-tree-sitter! 'sh-mode 'bash-ts-mode 'bash)
(set-tree-sitter! 'conf-toml-mode 'toml-ts-mode 'toml)
;; Plain HTML uses the native mixed HTML/CSS/JavaScript parser mode.
(add-to-list 'auto-mode-alist '("\\.html?\\'" . mhtml-mode))

;;; Clipboard

(defun akisarou/setup-clipboard ()
  "Use the desktop clipboard in terminal Emacs, preferring native Wayland."
  (when-let ((method
              (cond
               ((and (getenv "WAYLAND_DISPLAY")
                     (not (string-empty-p (getenv "WAYLAND_DISPLAY")))
                     (executable-find "wl-copy") (executable-find "wl-paste"))
                'wl-copy)
               ((and (getenv "DISPLAY")
                     (not (string-empty-p (getenv "DISPLAY")))
                     (executable-find "xsel"))
                'xsel))))
    (require 'xclip)
    (setq xclip-method method
          xclip-program (symbol-name method)
          select-enable-clipboard t
          select-enable-primary nil)
    (xclip-mode 1)))

(add-hook 'doom-first-input-hook #'akisarou/setup-clipboard)

;;; Directory editing

(defface akisarou/grease-ignored-face '((t :inherit shadow))
  "Ignored Git entries in Grease." :group 'grease)
(defface akisarou/grease-added-face '((t :inherit success))
  "Added and untracked Git entries in Grease." :group 'grease)
(defface akisarou/grease-modified-face '((t :inherit warning))
  "Modified Git entries in Grease." :group 'grease)
(defface akisarou/grease-deleted-face '((t :inherit error))
  "Deleted and conflicted Git entries in Grease." :group 'grease)

(defvar-local akisarou/grease-show-ignored nil
  "Whether Grease reveals ignored entries and hidden repository metadata.")
(defvar-local akisarou/grease-git-info nil
  "Git visibility and status tables for the last directory render.")

(defun akisarou/grease-git-output (&rest arguments)
  "Return successful Git output for ARGUMENTS in the displayed directory."
  (let ((default-directory grease--root-dir))
    (with-temp-buffer
      (when (eq 0 (apply #'process-file "git" nil '(t nil) nil arguments))
        (buffer-string)))))

(defun akisarou/grease-git-style (status)
  "Return a priority and face for porcelain STATUS."
  (cond
   ((or (string-match-p "U" status) (member status '("AA" "DD")))
    '(6 akisarou/grease-deleted-face))
   ((string-match-p "D" status) '(5 akisarou/grease-deleted-face))
   ((string-match-p "[MTRC]" status) '(4 akisarou/grease-modified-face))
   ((string-match-p "A" status) '(3 akisarou/grease-added-face))
   ((equal status "??") '(2 akisarou/grease-added-face))
   ((equal status "!!") '(1 akisarou/grease-ignored-face))
   (t '(0 grease-file-face))))

(defun akisarou/grease-read-git-info ()
  "Read source visibility and aggregate Git status by immediate child name."
  (when (and (not (file-remote-p grease--root-dir))
             (executable-find "git"))
    (require 'vc-git)
    (when-let* ((root (vc-git-root grease--root-dir))
                (sources (akisarou/grease-git-output
                          "ls-files" "--cached" "--others" "--exclude-standard" "-z"))
                (output (akisarou/grease-git-output
                         "--no-optional-locks" "status" "--porcelain=v1" "-z"
                         "--ignored=matching" "--untracked-files=normal" "--" ".")))
      (let ((visible (make-hash-table :test 'equal))
            (ignored (make-hash-table :test 'equal))
            (statuses (make-hash-table :test 'equal))
            (prefix (file-relative-name grease--root-dir root))
            (records (split-string output "\0" t)))
        (dolist (path (split-string sources "\0" t))
          (puthash (car (split-string path "/")) t visible))
        (when (equal prefix "./") (setq prefix ""))
        (while records
          (let* ((record (pop records))
                 (status (substring record 0 2))
                 (path (substring record 3)))
            ;; In -z output a rename/copy's destination precedes its old path.
            (when (string-match-p "[RC]" status) (pop records))
            (when (string-prefix-p prefix path)
              (let* ((relative (substring path (length prefix)))
                     (name (car (split-string relative "/")))
                     (previous (gethash name statuses)))
                ;; An ignored child does not make its parent directory ignored.
                (unless (and (equal status "!!")
                             (not (member relative (list name (concat name "/")))))
                  (when (equal status "!!") (puthash name t ignored))
                  (when (or (null previous)
                            (> (car (akisarou/grease-git-style status))
                               (car (akisarou/grease-git-style previous))))
                    (puthash name status statuses)))))))
        (list :visible visible :ignored ignored :statuses statuses)))))

(defun akisarou/grease-filter-files (files)
  "Filter ignored FILES before Grease records its editable baseline."
  (when (derived-mode-p 'grease-mode)
    (setq akisarou/grease-git-info (akisarou/grease-read-git-info)))
  (if (or (not (derived-mode-p 'grease-mode))
          akisarou/grease-show-ignored (null akisarou/grease-git-info))
      files
    (seq-remove
     (lambda (name)
       (if (string-prefix-p "." name)
           (not (gethash name (plist-get akisarou/grease-git-info :visible)))
         (gethash name (plist-get akisarou/grease-git-info :ignored))))
     files)))

(defun akisarou/grease-decorate (&rest _)
  "Display a path header and Git status without changing editable filenames."
  (remove-overlays (point-min) (point-max) 'akisarou-grease-display t)
  (save-excursion
    (goto-char (point-min))
    (let ((header (make-overlay (point) (line-end-position))))
      (overlay-put header 'akisarou-grease-display t)
      (overlay-put header 'display
                   (propertize (abbreviate-file-name grease--root-dir) 'face 'bold)))
    (while (zerop (forward-line 1))
      (when-let* ((entry (grease--get-line-data))
                  (name (plist-get entry :name))
                  (start (text-property-not-all (line-beginning-position)
                                                (line-end-position) 'grease-prefix t)))
        (let* ((status (and akisarou/grease-git-info
                            (gethash name (plist-get akisarou/grease-git-info :statuses))))
               (face (cadr (akisarou/grease-git-style (or status "  "))))
               (overlay (make-overlay start (line-end-position))))
          (overlay-put overlay 'akisarou-grease-display t)
          (overlay-put overlay 'evaporate t)
          (overlay-put overlay 'face
                       (if (eq (plist-get entry :type) 'dir) 'grease-directory-face face))
          (when akisarou/grease-git-info
            (overlay-put overlay 'before-string
                         (propertize (concat (or status "  ") " ") 'face face))))))))

(defun akisarou/grease-toggle-ignored ()
  "Toggle ignored entries while leaving tracked and new source dotfiles visible."
  (interactive)
  ;; A render rebuilds the native edit baseline. Never discard uncommitted edits.
  (when grease--buffer-dirty-p
    (user-error "Save directory edits before changing visibility"))
  (let ((name (plist-get (grease--get-line-data) :name)))
    (setq akisarou/grease-show-ignored (not akisarou/grease-show-ignored))
    (grease--render grease--root-dir t)
    (when name (grease--goto-file name)))
  (message "Ignored files: %s" (if akisarou/grease-show-ignored "shown" "hidden")))

(defun akisarou/grease-constrain-navigation ()
  "Keep normal-mode navigation on filenames, retaining the editable insertion row."
  (when (and (evil-normal-state-p) (not (grease--get-line-data)))
    (let ((target (save-excursion
                    (if (= (line-beginning-position) (point-min))
                        (progn (forward-line 1) (and (grease--get-line-data) (point)))
                      (skip-chars-forward " \t\n")
                      (when (eobp)
                        (skip-chars-backward " \t\n")
                        (and (grease--get-line-data) (line-beginning-position)))))))
      (when target
        (goto-char target)
        (grease--constrain-cursor)))))

(defun akisarou/entry-path (&optional no-error)
  "Return the selected Grease entry's path or the current file's path.
With NO-ERROR, return nil when point is not on an entry."
  (or (if (derived-mode-p 'grease-mode)
          (when-let* ((entry (grease--get-line-data))
                      (name (plist-get entry :name)))
            (expand-file-name name grease--root-dir))
        buffer-file-name)
      (unless no-error
        (user-error "No file or directory at point"))))

(defun akisarou/copy-path ()
  "Copy the selected entry's or current file's absolute path."
  (interactive)
  (kill-new (akisarou/entry-path)))

(defun akisarou/copy-filename ()
  "Copy the selected entry's or current file's filename."
  (interactive)
  (kill-new (file-name-nondirectory (akisarou/entry-path))))

(defun akisarou/grease-project-root ()
  "Open Grease at the current project's root."
  (interactive)
  (grease-open (akisarou/search-directory)))

(defun akisarou/grease-entry-details ()
  "Describe the selected entry without changing Grease's editable listing."
  (when-let* ((path (akisarou/entry-path t))
              (attributes (file-attributes path 'string)))
    (format " %s  %s  %s"
            (file-attribute-modes attributes)
            (file-size-human-readable (file-attribute-size attributes))
            (format-time-string "%Y-%m-%d %H:%M"
                                (file-attribute-modification-time attributes)))))

(defun akisarou/grease-toggle-details ()
  "Toggle a header showing the selected entry's permissions, size, and date."
  (interactive)
  (setq-local header-line-format
              (unless header-line-format
                '(:eval (akisarou/grease-entry-details))))
  (force-mode-line-update))

(defun akisarou/style-grease-buffer ()
  "Display Grease like an ordinary editing buffer, matching Oil."
  ;; Solaire normally recolors non-file buffers. Its public predicate is local
  ;; here so directory editing keeps the editor background after theme changes.
  (setq-local solaire-mode-real-buffer-fn #'always)
  (when (bound-and-true-p solaire-mode)
    (solaire-mode -1))
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
  (add-hook 'post-command-hook #'akisarou/grease-constrain-navigation nil t))

(use-package! grease
  :commands (grease-open grease-toggle grease-here)
  :hook (grease-mode . akisarou/style-grease-buffer)
  :init
  (setq grease-sort-method 'type
        grease-show-hidden t
        grease-use-icons nil
        grease-skip-confirm-for-simple-edits t
        grease-line-format "truncate"
        grease-preview-window-width 0.4)
  (map! :leader
        :desc "Open parent directory" "e" #'grease-toggle)
  :config
  ;; Grease currently has no entry filter or render hooks. These two adapters
  ;; leave its identity tracking, transaction handling, and filenames intact.
  (advice-add 'grease--filter-hidden :filter-return #'akisarou/grease-filter-files)
  (advice-add 'grease--render :after #'akisarou/grease-decorate)
  (map! :map grease-mode-map
        :n "C-e" #'grease-visit
        :n "C-f" #'grease-up-directory
        :n "q" #'grease-quit
        :n "ge" #'akisarou/grease-project-root
        :n "gd" #'akisarou/grease-toggle-details
        :n "g." #'akisarou/grease-toggle-ignored))

;;; Files and buffers

(defvar-local akisarou/save-in-progress nil
  "Non-nil while the current buffer is being formatted for saving.")

(defvar-local akisarou/after-save-functions nil
  "Functions to call after the current save and formatting have finished.")

(defun akisarou/finish-save (buffer error &optional formatted)
  "Finish saving BUFFER, then run its continuations unless ERROR occurred."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((continuations (nreverse akisarou/after-save-functions)))
        (setq akisarou/save-in-progress nil
              akisarou/after-save-functions nil)
        (if error
            (display-warning 'dotfiles
                             (format "Formatting %s failed: %s"
                                     (buffer-name) (error-message-string error)))
          ;; The initial write already saved the user's edits. Save the formatted
          ;; result without starting another format-on-save operation.
          (let ((apheleia-mode nil))
            (save-buffer))
          (when formatted
            (run-hooks 'apheleia-post-format-hook))
          (mapc #'funcall continuations))))))

(defun akisarou/save-file-buffer (&optional continuation)
  "Save and format the current file buffer, then call CONTINUATION.
Concurrent requests share the in-flight format operation. A failed formatter
leaves the buffer open and its unformatted edits saved on disk."
  (when (and buffer-read-only (buffer-modified-p))
    (user-error "Buffer is read-only"))
  (when continuation
    (push continuation akisarou/after-save-functions))
  (unless akisarou/save-in-progress
    (setq akisarou/save-in-progress t)
    (condition-case error
        (let* ((buffer (current-buffer))
               (formatters (and (buffer-modified-p)
                                (bound-and-true-p apheleia-mode)
                                (not (buffer-narrowed-p))
                                (apheleia--get-formatters))))
          (let ((apheleia-mode nil))
            (save-buffer))
          (if formatters
              (apheleia-format-buffer
               formatters nil
               :callback (cl-function
                          (lambda (&key error &allow-other-keys)
                            (akisarou/finish-save buffer error t))))
            (akisarou/finish-save buffer nil)))
      (error
       (setq akisarou/save-in-progress nil
             akisarou/after-save-functions nil)
       (signal (car error) (cdr error))))))

(defun akisarou/save-and-normal ()
  "Save the current buffer and leave Evil insert or visual state."
  (interactive)
  (if buffer-file-name
      (akisarou/save-file-buffer)
    (save-buffer))
  (when (bound-and-true-p evil-local-mode)
    (akisarou/escape)))

(defun akisarou/autosave-files ()
  "Save modified local files without prompting or overwriting external edits."
  (unless (active-minibuffer-window)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and buffer-file-name
                   (buffer-modified-p)
                   (not buffer-read-only)
                   (not akisarou/save-in-progress)
                   (not (file-remote-p buffer-file-name))
                   (file-writable-p buffer-file-name)
                   (verify-visited-file-modtime buffer))
          (condition-case error
              (akisarou/save-file-buffer)
            (error (display-warning 'dotfiles (error-message-string error)))))))))

(defun akisarou/autosave-on-focus-change ()
  "Save files when no Emacs frame has focus."
  (unless (seq-some #'frame-focus-state (frame-list))
    (akisarou/autosave-files)))

(add-hook 'doom-switch-buffer-hook #'akisarou/autosave-files)
(add-hook 'doom-switch-window-hook #'akisarou/autosave-files)
(add-function :after after-focus-change-function #'akisarou/autosave-on-focus-change)

(defun akisarou/file-buffers ()
  "Return all visited file buffers, matching the global buffer picker."
  (seq-filter #'buffer-file-name (buffer-list)))

(defun akisarou/cycle-file-buffer (direction)
  "Save the current file and move DIRECTION through file buffer history."
  (when buffer-file-name
    (akisarou/save-file-buffer))
  (let ((switch-to-prev-buffer-skip
         (lambda (_window buffer _bury-or-kill)
           (not (buffer-file-name buffer)))))
    (if (> direction 0) (next-buffer) (previous-buffer))))

(defun akisarou/close-file-buffers (buffers)
  "Save and close BUFFERS, preserving windows and unfinished formatter work."
  (dolist (buffer buffers)
    (when (buffer-live-p buffer)
      (let ((target buffer))
        (with-current-buffer target
          (akisarou/save-file-buffer
           (lambda ()
             (when (buffer-live-p target)
               (doom/kill-this-buffer-in-all-windows target)))))))))

(defun akisarou/close-buffer ()
  "Save and close the current file buffer, or close a non-file buffer normally."
  (interactive)
  (if buffer-file-name
      (akisarou/close-file-buffers (list (current-buffer)))
    (kill-current-buffer)))

(defun akisarou/close-other-file-buffers ()
  "Save and close all other file buffers, preserving windows and utility buffers."
  (interactive)
  (akisarou/close-file-buffers
   (delq (current-buffer) (akisarou/file-buffers))))

(defun akisarou/close-all-file-buffers ()
  "Save and close all file buffers."
  (interactive)
  (akisarou/close-file-buffers (akisarou/file-buffers)))

(map! "C-s" #'akisarou/save-and-normal)

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

;;; Completion

(defun akisarou/setup-minibuffer ()
  "Apply minibuffer styling and bindings above package prefix maps."
  (akisarou/remap-command-line-bg-h)
  ;; General's public buffer-local override takes precedence over minor modes
  ;; such as persp-mode, whose C-c p prefix shadows ordinary minibuffer maps.
  (let ((files (eq this-command 'akisarou/find-project-file))
        (revisions (eq this-command 'akisarou/git-file-history)))
    (general-define-key :keymaps 'local
                        "C-c" #'abort-recursive-edit
                        "C-x" (when (eq this-command 'consult-buffer)
                                #'akisarou/consult-kill-buffer)
                        "C-g" (cond (files #'akisarou/file-finder-toggle-ignored)
                                    (revisions #'akisarou/git-history-diff))
                        "C-h" (when files #'akisarou/file-finder-toggle-hidden)
                        "C-o" (when revisions #'akisarou/git-history-restore)
                        "C-w" (when revisions #'akisarou/git-history-worktree))))

(add-hook 'minibuffer-setup-hook #'akisarou/setup-minibuffer)

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

;;; Modal editing

(defun akisarou/navigate-window (direction)
  "Select an Emacs window in DIRECTION, continuing into tmux at the edge."
  (require 'windmove)
  (let* ((windmove-wrap-around nil)
         (window (windmove-find-other-window direction))
         (pane (getenv "TMUX_PANE")))
    (cond
     ((and (window-live-p window) (not (window-minibuffer-p window)))
      (select-window window))
     ((and (not (display-graphic-p)) (getenv "TMUX") pane (executable-find "tmux"))
      (unless (eq 0 (process-file "tmux" nil nil nil "select-pane" "-t" pane
                                  (alist-get direction '((left . "-L") (down . "-D")
                                                         (up . "-U") (right . "-R")))))
        (user-error "Could not select tmux pane %s" direction))))))

(after! evil
  (setopt evil-want-C-i-jump t)
  (setq evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'box
        evil-replace-state-cursor 'hbar
        evil-motion-state-cursor 'box
        evil-emacs-state-cursor 'hbar
        evil-kill-on-visual-paste nil
        evil-split-window-below t
        evil-vsplit-window-right t
        +evil-want-o/O-to-continue-comments nil)

  (evil-define-command akisarou/escape ()
    "Cancel an active minibuffer, or leave snippets and return to normal state."
    :repeat abort
    :suppress-operator t
    (if (minibufferp)
        (abort-recursive-edit)
      (when (bound-and-true-p yas-minor-mode)
        (save-excursion (yas-exit-all-snippets)))
      (evil-force-normal-state)))

  (evil-define-operator akisarou/delete-char (beg end type)
    "Delete the character range BEG to END without changing yank registers."
    :motion evil-forward-char
    (interactive "<R>")
    (evil-delete beg end type ?_))

  (evil-define-motion akisarou/search-next (count)
    "Move forward COUNT search matches, regardless of the search direction."
    :type exclusive
    (let ((evil-ex-search-direction 'forward))
      (evil-ex-search-next count)))

  (evil-define-motion akisarou/search-previous (count)
    "Move backward COUNT search matches, regardless of the search direction."
    :type exclusive
    (let ((evil-ex-search-direction 'forward))
      (evil-ex-search-previous count)))

  (defun akisarou/punctuation-undo-boundary ()
    "End the current Evil undo step after inserting punctuation."
    (when (and (evil-insert-state-p) (memq last-command-event '(?, ?. ?\;)))
      (evil-end-undo-step)
      (evil-start-undo-step)))

  (add-hook 'post-self-insert-hook #'akisarou/punctuation-undo-boundary)

  (defun akisarou/avoid-final-newline ()
    "Keep normal-mode point off the phantom row after a file's final newline."
    (when (and (evil-normal-state-p) (not (minibufferp))
               (eobp) (bolp) (> (point) (point-min)))
      (forward-line -1)
      (move-to-column (if (integerp temporary-goal-column) temporary-goal-column 0))
      (evil-adjust-cursor)))

  (add-hook 'post-command-hook #'akisarou/avoid-final-newline)

  (map! :ivro "C-c" #'akisarou/escape
        ;; Reserve this prefix before an LSP client attaches. Doom otherwise
        ;; interprets grr as its evaluate-region operator applied to a line.
        :nv "gr" nil
        :n "grr" #'+lookup/references
        :n "gri" #'+lookup/implementations
        :n "grt" #'+lookup/type-definition
        :nv "gra" #'akisarou/code-actions
        :nv "C-n" #'evil-mc-make-and-goto-next-match
        :nv "C-p" #'evil-mc-make-and-goto-prev-match
        :nv "C-x" #'evil-mc-skip-and-goto-next-match
        :n "x" #'akisarou/delete-char
        :m "n" #'akisarou/search-next
        :m "N" #'akisarou/search-previous
        :n "C-h" (cmd! (akisarou/navigate-window 'left))
        :n "C-j" (cmd! (akisarou/navigate-window 'down))
        :n "C-k" (cmd! (akisarou/navigate-window 'up))
        :n "C-l" (cmd! (akisarou/navigate-window 'right))
        ;; Terminal C-i and TAB share an event; keep jump history in normal mode.
        :n "C-i" #'evil-jump-forward
        :n [tab] #'evil-jump-forward
        :v "C-j" (cmd! (evil-next-line 6))
        :v "C-k" (cmd! (evil-previous-line 6))
        :n "C-<up>" (cmd! (enlarge-window 2))
        :n "C-<down>" (cmd! (shrink-window 2))
        :n "C-<left>" (cmd! (shrink-window-horizontally 2))
        :n "C-<right>" (cmd! (enlarge-window-horizontally 2))
        :n "[b" (cmd! (akisarou/cycle-file-buffer -1))
        :n "]b" (cmd! (akisarou/cycle-file-buffer 1)))

  (defun akisarou/terminal-cursor-update-h ()
    (unless (display-graphic-p)
      (send-string-to-terminal
       (cond
        ((evil-insert-state-p) "\e[6 q")
        ((evil-emacs-state-p) "\e[5 q")
        (t "\e[2 q")))))

  (add-hook 'post-command-hook #'akisarou/terminal-cursor-update-h))

(after! evil-mc
  ;; Fall through to the same match-selection bindings before and after cursors
  ;; activate. Doom otherwise changes C-n/C-p to cycle existing cursors.
  (map! :map evil-mc-key-map :nv "C-n" nil :nv "C-p" nil)
  ;; Our C-c escape must finish insert mode at every cursor before Doom's escape
  ;; hook clears them, just as Evil's native Escape does.
  (setf (alist-get 'akisarou/escape evil-mc-custom-known-commands)
        '((:default . evil-mc-execute-default-evil-normal-state))
        (alist-get 'akisarou/save-and-normal evil-mc-custom-known-commands)
        '((:default . evil-mc-execute-default-evil-normal-state))
        (alist-get 'akisarou/delete-char evil-mc-custom-known-commands)
        '((:default . evil-mc-execute-default-evil-delete))))

(use-package! tab-jump-out
  :hook (prog-mode . tab-jump-out-mode)
  :config
  (let ((binding
         '(menu-item "Jump past delimiter" tab-jump-out :filter
           (lambda (command)
             (when (and (bound-and-true-p evil-local-mode)
                        (evil-insert-state-p)
                        (not (bound-and-true-p yas--active-field-overlay))
                        (not (bound-and-true-p corfu--candidates)))
               command)))))
    ;; Handle both terminal TAB and GUI <tab>, with native fallback for indent.
    (define-key tab-jump-out-mode-map (kbd "TAB") binding)
    (define-key tab-jump-out-mode-map [tab] binding)))

;; Reserve the s prefix for surround commands, using Vim's regular f/t motions.
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(remove-hook 'doom-first-input-hook #'evil-snipe-override-mode)
(after! evil-snipe
  (evil-snipe-mode -1)
  (evil-snipe-override-mode -1))

(after! evil-surround
  (evil-define-command akisarou/surround-delete (char)
    "Delete the surrounding delimiter CHAR."
    :repeat t
    (interactive (evil-surround-input-char))
    (evil-surround-delete char))

  (evil-define-command akisarou/surround-change (char)
    "Replace the surrounding delimiter CHAR."
    :repeat t
    (interactive (evil-surround-input-char))
    (evil-surround-change char))

  ;; Reuse Evil's operators so counts, motions, and dot-repeat keep working.
  (map! :n "s" nil
        :n "sa" #'evil-surround-region
        :v "sa" #'evil-surround-region
        :n "sd" #'akisarou/surround-delete
        :n "sr" #'akisarou/surround-change))

;;; In-buffer completion

(after! corfu
  (setq corfu-preselect 'first
        corfu-cycle t
        corfu-quit-no-match nil
        corfu-quit-at-boundary nil
        ;; Vanilla bound RET to fall through. Doom exposes that policy directly:
        ;; C-e accepts a candidate; RET executes the typed Ex command or newline.
        +corfu-want-ret-to-confirm nil)
  (define-key corfu-map (kbd "C-e") #'corfu-insert)
  (evil-define-key* '(insert replace) corfu-map
    (kbd "C-e") #'corfu-insert)
  (evil-define-key* '(insert replace) corfu-mode-map
    (kbd "C-s") #'akisarou/save-and-normal)
  (require 'cape))

(after! corfu-auto
  ;; Trigger immediately after the first character so lsp-mode can cache the
  ;; broad candidate list before the input becomes a non-contiguous pattern.
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 1))

(after! corfu-popupinfo
  (setq corfu-popupinfo-delay '(0.2 . 0.1))
  (map! :map corfu-popupinfo-map
        "C-b" #'corfu-popupinfo-scroll-down
        "C-f" #'corfu-popupinfo-scroll-up))

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

;;; Language servers and formatting

;; Share existing tools with Neovim. System/project executables retain priority;
;; lsp-mode can provision its own copy when neither location supplies a server.
(let ((directory (expand-file-name "~/.local/share/nvim/mason/bin")))
  (when (file-directory-p directory)
    (add-to-list 'exec-path directory t)
    (unless (member directory (split-string (getenv "PATH") (regexp-quote path-separator) t))
      (setenv "PATH" (concat (getenv "PATH") path-separator directory)))))

;; Load the client API before defining helpers that extend its typed records.
(require 'lsp-mode)

(defconst akisarou/language-servers
  '(tsgo lua-language-server json-ls html-ls css-ls tailwindcss cssmodules-ls
    dockerfile-ls docker-compose-ls yamlls mdx-analyzer npm-workspaces-ls
    taplo rust-analyzer clangd just-lsp zshcs)
  "Language servers to ensure, corresponding to the Neovim server setup.")

(defun akisarou/ensure-language-servers ()
  "Provision missing servers using lsp-mode's asynchronous installers."
  (interactive)
  (require 'lsp-mode)
  (dolist (server akisarou/language-servers)
    (lsp-ensure-server server))
  (akisarou/ensure-json-schemas))

(add-hook 'doom-first-input-hook
          (defun akisarou/ensure-language-servers-h ()
            (unless noninteractive (akisarou/ensure-language-servers)))
          t)

(defun akisarou/register-npm-language-server (id package binary arguments activation)
  "Register an add-on server ID with native npm provisioning and ACTIVATION."
  (lsp-dependency id `(:system ,binary) `(:npm :package ,package :path ,binary))
  (lsp-register-client
   (make-lsp-client
    :server-id id :add-on? t :priority -1 :activation-fn activation
    :new-connection
    (lsp-stdio-connection (lambda () (cons (lsp-package-path id) arguments)))
    :download-server-fn
    (lambda (_client callback error-callback _update?)
      (lsp-package-ensure id callback error-callback)))))

(defun akisarou/npm-workspace-json-p (file _mode)
  "Enable dependency completions for package.json inside an npm/Yarn/pnpm workspace."
  (and (equal (file-name-nondirectory file) "package.json")
       (locate-dominating-file
        file
        (lambda (directory)
          (or (file-exists-p (expand-file-name "pnpm-workspace.yaml" directory))
              (let ((package (expand-file-name "package.json" directory)))
                (when (and (file-readable-p package)
                           (seq-some (lambda (name)
                                       (file-exists-p (expand-file-name name directory)))
                                     '("yarn.lock" "package-lock.json" "lerna.json" "rush.json")))
                  (ignore-errors
                    (with-temp-buffer
                      (insert-file-contents package)
                      (plist-get (json-parse-buffer :object-type 'plist) :workspaces))))))))))

(defun akisarou/add-language-server-installer (id dependency arguments)
  "Extend the maintained client ID with a DEPENDENCY installer and ARGUMENTS."
  (let ((client (copy-lsp--client (gethash id lsp-clients))))
    (setf (lsp--client-new-connection client)
          (lsp-stdio-connection
           (lambda () (cons (lsp-package-path dependency) arguments)))
          (lsp--client-download-server-fn client)
          (lambda (_client callback error-callback _update?)
            (lsp-package-ensure dependency callback error-callback)))
    (lsp-register-client client)))

(after! lsp-mode
  (akisarou/register-npm-language-server
   'cssmodules-ls "cssmodules-language-server" "cssmodules-language-server" nil
   (let ((javascript-p (lsp-activate-on "javascript" "javascriptreact"
                                      "typescript" "typescriptreact")))
     (lambda (file mode)
       (and (locate-dominating-file file "package.json")
            (funcall javascript-p file mode)))))
  (akisarou/register-npm-language-server
   'docker-compose-ls "@microsoft/compose-language-service" "docker-compose-langserver"
   '("--stdio")
   (lambda (file _mode)
     (and (derived-mode-p 'yaml-mode 'yaml-ts-mode)
          (string-match-p
           "/\\(?:docker-compose\\|compose\\)\\(?:\\.[^.]+\\)*\\.ya?ml\\'" file))))
  (akisarou/register-npm-language-server
   'npm-workspaces-ls "npm-workspaces-language-server" "npm-workspaces-lsp"
   '("--stdio") #'akisarou/npm-workspace-json-p)

  (lsp-dependency 'zshcs '(:system "zshcs")
                  '(:cargo :package "zshcs" :path "zshcs"
                           :git "https://github.com/yuys13/zshcs"))
  (lsp-register-client
   (make-lsp-client
    :server-id 'zshcs
    :activation-fn (lambda (_file _mode)
                     (and (derived-mode-p 'sh-mode) (eq sh-shell 'zsh)))
    :new-connection (lsp-stdio-connection (lambda () (list (lsp-package-path 'zshcs))))
    :download-server-fn
    (lambda (_client callback error-callback _update?)
      (lsp-package-ensure 'zshcs callback error-callback)))))

(after! lsp-toml
  (lsp-dependency 'taplo-cli '(:system "taplo")
                  '(:cargo :package "taplo-cli" :path "taplo"))
  (akisarou/add-language-server-installer 'taplo 'taplo-cli '("lsp" "stdio")))

(after! lsp-just
  (lsp-dependency 'just-language-server '(:system "just-lsp")
                  '(:cargo :package "just-lsp" :path "just-lsp"))
  (akisarou/add-language-server-installer 'just-lsp 'just-language-server nil))

(use-package! just-mode
  :mode ("/\\.?[Jj]ustfile\\'" "\\.just\\'")
  :hook (just-mode-local-vars . lsp!))

(add-hook! 'sh-mode-local-vars-hook :append
  (defun akisarou/lsp-zsh-h ()
    (when (eq sh-shell 'zsh)
      (require 'lsp-mode)
      (setq-local lsp-language-id-configuration
                  (cons '(sh-mode . "zsh") lsp-language-id-configuration))
      (lsp!))))

(add-hook! '(dockerfile-ts-mode-local-vars-hook conf-toml-mode-local-vars-hook
             toml-ts-mode-local-vars-hook) :append #'lsp!)

(after! lsp-css
  (setq lsp-css-validate t lsp-css-lint-unknown-at-rules "ignore")
  (lsp-register-custom-settings
   '(("scss.validate" lsp-css-validate t)
     ("scss.lint.unknownAtRules" lsp-css-lint-unknown-at-rules))))

(after! lsp-html
  (setf (lsp--client-initialization-options (gethash 'html-ls lsp-clients))
        (lambda ()
          (list :dataPaths lsp-html-custom-data :provideFormatter t
                :embeddedLanguages '(:css t :javascript t)
                :configurationSection ["html" "css" "javascript"]))))

(defconst akisarou/extra-json-schemas
  '((:fileMatch ["eas.json"]
     :url "https://raw.githubusercontent.com/expo/eas-cli/main/packages/eas-json/schema/eas.schema.json")
    (:fileMatch ["launch.json"]
     :url "https://codeberg.org/mfussenegger/dapconfig-schema/raw/branch/master/dapconfig-schema.json")))

(defun akisarou/load-json-schema-catalog ()
  "Load the cached SchemaStore catalog and update active JSON workspaces."
  (when-let ((file (lsp-package-path 'json-schema-store)))
    (let ((catalog (condition-case error
                       (with-temp-buffer
                         (insert-file-contents file)
                         (json-parse-buffer :object-type 'plist :array-type 'array))
                     (json-error
                      (delete-file file)
                      (lsp--warn "Discarding invalid SchemaStore cache: %s" error)
                      nil)))
          workspaces)
      (setq lsp-json-schemas
            (vconcat (seq-filter (lambda (schema) (plist-get schema :fileMatch))
                                 (plist-get catalog :schemas))
                     akisarou/extra-json-schemas))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (dolist (workspace (lsp-workspaces))
            (when (eq (lsp--workspace-server-id workspace) 'json-ls)
              (cl-pushnew workspace workspaces)))))
      (dolist (workspace workspaces)
        (with-lsp-workspace workspace
          (lsp--set-configuration (lsp-configuration-section "json")))))))

(defun akisarou/ensure-json-schemas ()
  "Download SchemaStore once through lsp-mode's dependency manager."
  (if (lsp-package-path 'json-schema-store)
      (akisarou/load-json-schema-catalog)
    (lsp-package-ensure 'json-schema-store #'akisarou/load-json-schema-catalog
                        (lambda (error) (lsp--warn "SchemaStore: %s" error)))))

(after! lsp-json
  (lsp-dependency
   'json-schema-store
   `(:download :url "https://www.schemastore.org/api/json/catalog.json"
               :store-path ,(expand-file-name "schema-store/catalog.json"
                                              lsp-server-install-dir)))
  (setq lsp-json-validate t lsp-json-schemas (vconcat akisarou/extra-json-schemas))
  (akisarou/load-json-schema-catalog))

(after! lsp-tailwindcss
  (setq lsp-tailwindcss-skip-config-check t
        lsp-tailwindcss-lint-css-conflict "error"
        lsp-tailwindcss-class-attributes ["class" "className" "style" "classList"]
        lsp-tailwindcss-class-functions ["cn" "clsx" "tw" "tw.color" "tw.style"]
        lsp-tailwindcss-major-modes
        '(js-mode js-ts-mode rjsx-mode typescript-mode typescript-ts-mode tsx-ts-mode
          html-mode html-ts-mode mhtml-mode mhtml-ts-mode web-mode css-mode css-ts-mode
          scss-mode less-css-mode markdown-mode markdown-ts-mode))
  (lsp-register-custom-settings '(("editor.tabSize" tab-width))))

(after! lsp-lua
  (setq lsp-lua-diagnostics-globals ["vim"])
  (let ((runtime (when (executable-find "nvim")
                   (car (process-lines "nvim" "--headless" "-u" "NONE" "-n"
                                       "-c" "lua io.write(vim.env.VIMRUNTIME)" "-c" "qa")))))
    (setq lsp-lua-workspace-library
          (vconcat (seq-filter #'file-directory-p
                               (delq nil (list (when runtime (expand-file-name "lua" runtime))
                                               (expand-file-name "~/.config/nvim/lua"))))))))

(after! lsp-yaml
  ;; The native server consumes SchemaStore's catalog directly.
  (setq lsp-yaml-schema-store-enable t)
  (lsp-register-custom-settings '(("redhat.telemetry.enabled" nil t))))

(after! lsp-rust
  (setq lsp-rust-analyzer-lens-references-adt-enable t
        lsp-rust-analyzer-lens-references-enum-variant-enable t
        lsp-rust-analyzer-lens-references-method-enable t
        lsp-rust-analyzer-lens-references-trait-enable t))

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
  (setq lsp-clients-tsgo-path "tsgo"
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

;;; Finders

(defun akisarou/search-directory ()
  "Use the current project root, or the current directory outside a project."
  (or (doom-project-root) default-directory))

(defun akisarou/file-finder-split (_input _style)
  "List all files once; let the completion style filter the entire input."
  '("." 0))

(defun akisarou/file-finder-toggle-ignored ()
  "Toggle ignored files, retaining the current file query."
  (interactive)
  (throw 'akisarou/file-finder-restart
         (list :toggle 'ignored :input (minibuffer-contents-no-properties))))

(defun akisarou/file-finder-toggle-hidden ()
  "Toggle dotfiles, retaining the current file query."
  (interactive)
  (throw 'akisarou/file-finder-restart
         (list :toggle 'hidden :input (minibuffer-contents-no-properties))))

(defun akisarou/file-finder-recents (root hidden ignored)
  "Return recent files in ROOT matching HIDDEN and IGNORED visibility."
  (let* ((recent
          (cl-loop for file in recentf-list
                   for path = (expand-file-name file)
                   for relative = (file-relative-name path root)
                   when (and (string-prefix-p root path) (file-regular-p path)
                             (not (string-match-p
                                   "\\(?:\\`\\|/\\)\\(?:\\.git\\|node_modules\\)\\(?:/\\|\\'\\)"
                                   relative))
                             (or hidden (not (string-match-p "\\(?:\\`\\|/\\)\\." relative))))
                   collect relative))
         (excluded
          (unless (or ignored (null recent))
            ;; One NUL-delimited Git query also handles spaces and newlines.
            (with-temp-buffer
              (dolist (file recent) (insert file "\0"))
              (let ((default-directory root))
                (call-process-region (point-min) (point-max) "git" t '(t nil) nil
                                     "check-ignore" "--no-index" "-z" "--stdin"))
              (split-string (buffer-string) "\0" t)))))
    (seq-difference recent excluded #'equal)))

(defun akisarou/find-project-file ()
  "Fuzzy-find project files, with recent files first.
C-g toggles ignored files; C-h toggles dotfiles.  C-c cancels."
  (interactive)
  (require 'consult)
  (require 'recentf)
  (recentf-mode 1)
  (let ((default-directory (file-name-as-directory (akisarou/search-directory)))
        (hidden t) ignored initial done result)
    (while (not done)
      (setq result
            (catch 'akisarou/file-finder-restart
              (akisarou/read-project-file hidden ignored initial)))
      (if (eq (car-safe result) :toggle)
          (progn
            (pcase (plist-get result :toggle)
              ('hidden (setq hidden (not hidden)))
              ('ignored (setq ignored (not ignored))))
            (setq initial (plist-get result :input)))
        (setq done t)))
    result))

(defun akisarou/read-project-file (hidden ignored initial)
  "Read a project file using HIDDEN and IGNORED visibility and INITIAL input."
  (let* (;; A visibility toggle re-enters the picker inside a different command.
         ;; Preserve its identity for minibuffer bindings and Vertico repeat.
         (this-command 'akisarou/find-project-file)
         (recent (akisarou/file-finder-recents default-directory hidden ignored))
         (recent-set (consult--string-hash recent))
         (consult-fd-args
          (list (or (executable-find "fd" 'remote)
                    (executable-find "fdfind" 'remote)
                    (user-error "Install fd to find project files"))
                "--color=never" "--type=file" "--follow"
                "--exclude=.git" "--exclude=node_modules" "--full-path"
                (and hidden "--hidden") (and ignored "--no-ignore")))
         (consult-async-split-style 'akisarou-files)
         (consult-async-split-styles-alist
          (cons '(akisarou-files :function akisarou/file-finder-split)
                consult-async-split-styles-alist)))
    ;; Consult's native multi-source pipeline gives recents immediate display,
    ;; then streams fd results. The ordinary fuzzy completion style does all
    ;; filtering, so no regexp prefix or minimum query length is needed.
    (consult--multi
     (list
      (list :name "Recent project files" :category 'file :items recent
            :history 'file-name-history :state #'consult--file-state :preview-key "C-SPC")
      (list :name "Project files" :category 'file
            :history 'file-name-history :state #'consult--file-state :preview-key "C-SPC"
            :async (consult--process-collection (consult--fd-make-builder '("."))
                     :min-input 0 :debounce 0 :throttle 0 :file-handler t
                     :transform
                     (consult--async-transform
                      (lambda (files)
                        (cl-loop for file in files
                                 for path = (string-remove-prefix "./" file)
                                 unless (gethash path recent-set) collect path))))))
     :prompt (format "[%s%s%s] Find file: "
                     (file-name-nondirectory (directory-file-name default-directory))
                     (if hidden "" " · no dotfiles") (if ignored " · ignored" ""))
     :initial initial
     :require-match t :sort nil :history 'file-name-history)))

(defun akisarou/search-files (&optional initial)
  "Search project files with ripgrep, optionally starting with INITIAL."
  (interactive)
  (consult-ripgrep (akisarou/search-directory) initial))

(defun akisarou/search-at-point (&optional big-word)
  "Search for the active selection or word at point.
With BIG-WORD, use the whitespace-delimited WORD instead of a symbol."
  (interactive)
  (let ((text
         (cond
          ((use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end)))
          (big-word
           (save-excursion
             (skip-chars-backward "^ \t\r\n")
             (let ((start (point)))
               (skip-chars-forward "^ \t\r\n")
               (buffer-substring-no-properties start (point)))))
          (t (thing-at-point 'symbol t)))))
    (unless (and text (not (string-empty-p text)))
      (user-error "No text to search for"))
    (akisarou/search-files
     (string-replace " " "\\ " (regexp-quote text)))))

(defun akisarou/find-todos ()
  "Search project files for TODO, PERF, NOTE, and FIX comments."
  (interactive)
  (akisarou/search-files (regexp-opt '("TODO" "PERF" "NOTE" "FIX:"))))

(defun akisarou/show-results ()
  "Show the most recent compilation or exported search results buffer."
  (interactive)
  (pop-to-buffer (next-error-find-buffer)))

(defvar embark-default-action-overrides)
(defvar embark-pre-action-hooks)
(defvar embark-quit-after-action)

(defun akisarou/consult-kill-buffer ()
  "Kill the selected buffer and refresh the picker, preserving its query."
  (interactive)
  (require 'embark)
  (let ((embark-default-action-overrides '((buffer . kill-buffer)))
        (embark-quit-after-action nil)
        ;; Keep kill-buffer's modified-buffer prompt; omit Embark's additional
        ;; confirmation for an ordinary buffer action from this picker.
        (embark-pre-action-hooks (cons '(kill-buffer) embark-pre-action-hooks)))
    (embark-dwim)))

(after! consult
  (setq consult-buffer-sources '(consult-source-buffer consult-source-hidden-buffer)
        ;; Keep logs, process buffers, dashboards, and scratch out of the normal
        ;; list. Consult's native SPC narrowing still reveals hidden buffers.
        consult-buffer-filter '("\\`[ *]")
        consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --hidden --glob !.git --glob !node_modules"))

(after! vertico
  (map! :map vertico-map
        :desc "Export results" "C-q" #'embark-export))

;;; Git history and temporary worktrees

(after! magit
  ;; Magit's revision-navigation g prefix shadows Evil's global gg motion.
  (map! :map magit-blob-mode-map :nv "gg" #'evil-goto-first-line)
  (map! :map magit-log-mode-map :n
        "C-g" #'akisarou/git-log-diff
        :n "C-w" #'akisarou/git-view))

(defvar akisarou/git-history-action 'visit
  "Dynamically bound action for the current file history picker.")

(defun akisarou/git-history-accept (action)
  "Accept the current revision with ACTION."
  (unless (and (>= vertico--index 0) (> vertico--total 0))
    (user-error "Select a revision first"))
  (setq akisarou/git-history-action action)
  (vertico-exit))

(defun akisarou/git-history-diff ()
  "Compare the selected file revision with HEAD."
  (interactive)
  (akisarou/git-history-accept 'diff))

(defun akisarou/git-history-restore ()
  "Restore the selected file revision using Magit's checkout command."
  (interactive)
  (akisarou/git-history-accept 'restore))

(defun akisarou/git-history-worktree ()
  "Browse the selected commit in a temporary worktree."
  (interactive)
  (akisarou/git-history-accept 'worktree))

(defun akisarou/git-log-diff ()
  "Compare the commit at point in a Magit log with HEAD."
  (interactive)
  (magit-diff-range (concat (or (magit-commit-at-point)
                              (user-error "No commit at point")) "..HEAD")))

(defvar akisarou/git-views nil
  "Temporary worktrees owned by this Emacs session, newest first.")

(defun akisarou/git-view-buffers (view)
  "Return buffers visiting files or directories in VIEW."
  (let ((directory (plist-get view :directory)))
    (seq-filter
     (lambda (buffer)
       (with-current-buffer buffer
         (or (and buffer-file-name
                  (string-prefix-p directory (expand-file-name buffer-file-name)))
             (and (derived-mode-p 'grease-mode 'magit-mode 'dired-mode)
                  (string-prefix-p directory (expand-file-name default-directory))))))
     (buffer-list))))

(defun akisarou/git-view-cleanup (view)
  "Remove an owned VIEW only if Git and its Emacs buffers are clean.
Return non-nil on success.  Edited worktrees remain available on disk."
  (let ((default-directory (plist-get view :root))
        (buffers (akisarou/git-view-buffers view)))
    ;; Plain worktree remove also refuses untracked files and submodules.
    ;; Never force deletion or fall back to recursively deleting the directory.
    (when (and (not (seq-some
                     (lambda (buffer)
                       (with-current-buffer buffer
                         (if (derived-mode-p 'grease-mode)
                             (or grease--buffer-dirty-p grease--pending-changes)
                           (and buffer-file-name (buffer-modified-p)))))
                     buffers))
               ;; Git normally permits removal of ignored files as well.
               ;; Retain those too, including newly generated build output.
               (not (let ((default-directory (plist-get view :directory)))
                      (magit-git-string "ls-files" "--others" "--directory")))
               (zerop (magit-call-git "worktree" "remove"
                                     (directory-file-name (plist-get view :directory)))))
      (dolist (buffer buffers) (kill-buffer buffer))
      (setq akisarou/git-views (delq view akisarou/git-views))
      t)))

(defun akisarou/git-view (&optional revision)
  "Browse REVISION in Grease using a temporary detached Git worktree.
In a Magit log, use the commit at point.  Elsewhere, prompt for a revision.
Use `akisarou/git-view-return' to return and remove the clean worktree."
  (interactive)
  (require 'magit)
  (let* ((root (or (magit-toplevel) (user-error "Not in a Git repository")))
         (revision (or revision (magit-commit-at-point)
                       (magit-read-branch-or-commit "Browse revision" "HEAD")))
         (commit (or (magit-rev-parse (concat revision "^{commit}"))
                     (user-error "Not a commit: %s" revision)))
         (directory (file-name-as-directory (make-temp-file "emacs-git-view-" t)))
         (view (list :directory directory :root root
                     :windows (current-window-configuration))))
    (let ((default-directory root))
      (unless (zerop (magit-call-git "worktree" "add" "--detach" directory commit))
        (when (directory-empty-p directory) (delete-directory directory))
        (user-error "Could not create a worktree for %s" revision)))
    (push view akisarou/git-views)
    (grease-open directory)
    (message "Browsing %s; SPC g v r returns to the original view" (substring commit 0 10))))

(defun akisarou/git-view-return ()
  "Return from the current temporary worktree and remove it when clean."
  (interactive)
  (let ((view (or (seq-find
                   (lambda (view)
                     (string-prefix-p (plist-get view :directory)
                                      (expand-file-name default-directory)))
                   akisarou/git-views)
                  (car akisarou/git-views)
                  (user-error "No temporary worktree to return from"))))
    (set-window-configuration (plist-get view :windows))
    (unless (akisarou/git-view-cleanup view)
      (message "Kept edited worktree at %s; return again after saving or discarding changes"
               (plist-get view :directory)))))

(defun akisarou/git-view-cleanup-on-exit ()
  "Clean up this session's unmodified temporary worktrees."
  (dolist (view (copy-sequence akisarou/git-views))
    (unless (akisarou/git-view-cleanup view)
      (message "Kept edited worktree at %s" (plist-get view :directory)))))

(add-hook 'kill-emacs-hook #'akisarou/git-view-cleanup-on-exit)

(defvar akisarou/git-file-history-history nil
  "Minibuffer history for file revisions.")

(defun akisarou/git-file-history-candidates (file)
  "Return display candidates paired with revision and historical path for FILE.
FILE is relative to the repository root in `default-directory'."
  (with-temp-buffer
    (unless (eq 0 (magit-git-insert
                   "log" "--follow" "--diff-filter=d" "--no-color" "--no-notes"
                   "--no-show-signature" "-z" "--name-only"
                   "--format=%H%x00%h%x00%as%x00%an%x00%s" "--" file))
      (user-error "Could not read file history"))
    ;; NUL fields preserve tabs and newlines in paths and empty commit subjects.
    (cl-loop for (rev short date author subject path)
             on (split-string (buffer-string) "\0")
             by (lambda (fields) (nthcdr 6 fields))
             while path
             collect (cons (format "%s  %s  %-20s  %s" short date
                                   (truncate-string-to-width author 20 nil nil t) subject)
                           (cons rev (string-remove-prefix "\n" path))))))

(defun akisarou/git-file-history-state (root)
  "Build a Consult preview state for file revisions in ROOT."
  (let ((state (consult--buffer-state))
        (existing (buffer-list))
        temporary)
    (lambda (action revision)
      ;; Actions other than visiting must restore the original buffer before
      ;; opening a diff, checking out a file, or browsing a worktree.
      (when (and (eq action 'return) (not (eq akisarou/git-history-action 'visit)))
        (setq revision nil))
      (let* ((default-directory root)
             (buffer (when revision
                       (magit-find-file-noselect (car revision) (cdr revision)
                                                 t (not (eq action 'return))))))
        (when (and buffer (not (memq buffer existing)))
          (cl-pushnew buffer temporary))
        (unwind-protect
            (funcall state action buffer)
          (when (eq action 'return)
            (dolist (preview temporary)
              (when (and (buffer-live-p preview) (not (eq preview buffer)))
                (kill-buffer preview)))))))))

(defun akisarou/git-file-history ()
  "Browse file history with read-only previews.
C-g diffs against HEAD; C-o restores a file; C-w browses a temporary worktree."
  (interactive)
  (require 'consult)
  (require 'magit)
  (require 'vc-git)
  (let* ((file (or buffer-file-name (bound-and-true-p magit-buffer-file-name)
                   (user-error "Buffer is not visiting a file")))
         (default-directory (or (vc-git-root file)
                                (user-error "File is not in a Git repository")))
         (relative (file-relative-name file default-directory))
         (candidates (akisarou/git-file-history-candidates relative))
         (akisarou/git-history-action 'visit))
    (unless candidates (user-error "No Git history for this file"))
    (let* ((revision
            (consult--read candidates
                           :prompt "File history: " :sort nil :require-match t
                           :category 'git-commit :history 'akisarou/git-file-history-history
                           :lookup #'consult--lookup-cdr :preview-key '(:debounce 0.1 any)
                           :state (akisarou/git-file-history-state default-directory)))
           (commit (car revision))
           (path (cdr revision)))
      (pcase akisarou/git-history-action
        ('diff (magit-diff-range (concat commit "..HEAD") nil
                                (delete-dups (list relative path))))
        ('restore
         (when (yes-or-no-p (format "Restore %s from %s in working tree and index? "
                                   path (substring commit 0 10)))
           (when (zerop (magit-file-checkout commit path))
             (when-let ((buffer (get-file-buffer (expand-file-name path))))
               (with-current-buffer buffer (revert-buffer t t)))
             (find-file (expand-file-name path)))))
        ('worktree (akisarou/git-view commit))))))

;;; Diagnostics and language actions

(defun akisarou/jump-diagnostic (count &optional level)
  "Jump COUNT diagnostics, wrapping around and optionally matching LEVEL exactly."
  (require 'flycheck)
  (let ((errors
         (sort (seq-filter
                (lambda (error)
                  (and (or (null level) (eq level (flycheck-error-level error)))
                       (flycheck-error-pos error)))
                flycheck-current-errors)
               (lambda (a b) (< (flycheck-error-pos a) (flycheck-error-pos b))))))
    (unless errors
      (user-error "No %sdiagnostics in this buffer" (if level (format "%s " level) "")))
    (dotimes (_ (abs count))
      (let* ((ordered (if (< count 0) (reverse errors) errors))
             (next (or (seq-find
                        (lambda (error)
                          (if (< count 0)
                              (< (flycheck-error-pos error) (point))
                            (> (flycheck-error-pos error) (point))))
                        ordered)
                       (car ordered))))
        (flycheck-jump-to-error next)))
    (flycheck-display-error-at-point)))

(defconst akisarou/hidden-code-action-titles
  '("Inline variable" "Move to" "Extract" "Change to parameter" "Convert"
    "Add missing function declaration" "Add all missing function declarations"
    "Generate 'get'" "Add braces" "Remove braces" "Remove unused"
    "Fix all detected spelling" "Change spelling to" "Convert named export"
    "Disable react/jsx-no-undef")
  "Literal code action title fragments hidden by the action picker.")

(defun akisarou/code-actions ()
  "Choose a code action using the same title filters as the Neovim config."
  (interactive)
  (let* ((actions
          (seq-remove
           (lambda (action)
             (let ((title (lsp:code-action-title action)))
               (seq-some (lambda (fragment)
                           (string-match-p (regexp-quote fragment) title))
                         akisarou/hidden-code-action-titles)))
           (lsp-code-actions-at-point)))
         ;; Include an index so equally named actions remain individually selectable.
         (candidates (cl-loop for action in (seq-into actions 'list)
                              for index from 1
                              collect (cons (format "%d. %s" index (lsp:code-action-title action))
                                            action))))
    (unless candidates
      (user-error "No matching code actions"))
    (lsp-execute-code-action
     (if (= (length candidates) 1)
         (cdar candidates)
       (cdr (assoc (completing-read "Code action: " candidates nil t) candidates))))))

(defun akisarou/remove-unused-imports ()
  "Remove unused imports, then format and save the current file."
  (interactive)
  (lsp-execute-code-action-by-kind "source.removeUnusedImports")
  (akisarou/save-file-buffer))

(after! lsp-mode
  (map! :map lsp-mode-map
        :n "gd" #'lsp-find-definition
        :n "gD" #'lsp-find-declaration
        :n "C-w C-f" #'consult-flycheck))

;;; Neovim workflow bindings

;; These replace Doom's help prefix, universal argument, and scratch shortcut.
;; Help remains available through SPC H (and F1).
(define-key doom-leader-map (kbd "h") nil)
(define-key doom-leader-map (kbd "u") nil)
(define-key doom-leader-map (kbd "x") nil)

(map! :leader
      :desc "Help" "H" help-map
      :desc "Clear search highlights" "h" #'evil-ex-nohighlight
      (:prefix ("f" . "find")
       :desc "Commands" "a" #'execute-extended-command
       :desc "Find project file" "f" #'akisarou/find-project-file
       :desc "Recent files" "r" #'consult-recent-file
       :desc "Search files" "s" #'akisarou/search-files
       :desc "Resume search" "l" #'vertico-repeat
       :desc "Search word or selection" "w" #'akisarou/search-at-point
       :desc "Search WORD" "W" (cmd! (akisarou/search-at-point t))
       :desc "Search buffer" "b" #'consult-line
       :desc "Buffers" "e" #'consult-buffer
       :desc "Buffer diagnostics" "d" #'consult-flycheck
       :desc "Result locations" "q" #'consult-compile-error
       :desc "Man pages" "m" #'consult-man
       :desc "Buffer symbols" "t" #'consult-imenu
       :desc "TODO comments" "x" #'akisarou/find-todos)
      (:prefix ("b" . "buffer")
       :desc "Save and close buffer" "d" #'akisarou/close-buffer
       :desc "Close other file buffers" "o" #'akisarou/close-other-file-buffers
       :desc "Save and close file buffers" "a" #'akisarou/close-all-file-buffers)
      (:prefix ("g" . "git")
       :desc "Branches" "b" #'magit-branch-checkout
       :desc "Commit history" "c" #'magit-log-current
       :desc "File history" "f" #'akisarou/git-file-history
       (:prefix ("v" . "revision worktree")
        :desc "Browse revision" "v" #'akisarou/git-view
        :desc "Return from revision" "r" #'akisarou/git-view-return))
      "p f" nil
      (:prefix ("u" . "undo")
       :desc "Undo history" "u" #'vundo)
      (:prefix ("x" . "results")
       :desc "Show results" "q" #'akisarou/show-results)
      (:prefix ("y" . "copy")
       :desc "Copy entry path" "p" #'akisarou/copy-path
       :desc "Copy entry filename" "f" #'akisarou/copy-filename)
      (:prefix ("c" . "code")
       :desc "Restart language server" "l" #'lsp-workspace-restart
       (:prefix ("q" . "cleanup")
        :desc "Remove unused imports" "i" #'akisarou/remove-unused-imports)))

(map! :n "]d" (cmd! (akisarou/jump-diagnostic (or evil-count 1)))
      :n "[d" (cmd! (akisarou/jump-diagnostic (- (or evil-count 1))))
      :n "]e" (cmd! (akisarou/jump-diagnostic (or evil-count 1) 'error))
      :n "[e" (cmd! (akisarou/jump-diagnostic (- (or evil-count 1)) 'error))
      :n "]w" (cmd! (akisarou/jump-diagnostic (or evil-count 1) 'warning))
      :n "[w" (cmd! (akisarou/jump-diagnostic (- (or evil-count 1)) 'warning)))

;;; Agent commands

(use-package! codex
  :config
  (define-key doom-leader-map (kbd "a") nil)
  (map! :leader
        (:prefix ("a" . "ai")
         :desc "Codex" "a" codex-command-map)))
