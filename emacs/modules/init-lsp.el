;;; init-lsp.el --- LSP Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; LSP Configuration

;;; Code:


;;; MASON
(use-package mason
  :ensure t
  :config
  ;; List of packages to ensure
  (defvar my-mason-packages
	'("vtsls"
	  "lua-language-server"
	  "json-lsp"
	  "html-lsp"
	  "css-lsp"
	  "gopls"
	  "tailwindcss-language-server"
	  "cssmodules-language-server"
	  "docker-language-server"
	  "docker-compose-language-service"
	  "yaml-language-server"
	  "clangd"
	  "mdx_analyzer"
	  "prettierd"
	  "taplo"))

  ;; Initialize Mason and install packages
  (mason-ensure
   (lambda ()
	 (dolist (pkg my-mason-packages)
	   (ignore-errors (mason-install pkg))))))


;;; LSP
;; Emacs comes with an integrated LSP client called `eglot', which offers basic LSP functionality.
;; However, `eglot' has limitations, such as not supporting multiple language servers
;; simultaneously within the same buffer (e.g., handling both TypeScript, Tailwind and ESLint
;; LSPs together in a React project). For this reason, the more mature and capable
;; `lsp-mode' is included as a third-party package, providing advanced IDE-like features
;; and better support for multiple language servers and configurations.
;;
;; NOTE: To install or reinstall an LSP server, use `M-x install-server RET`.
;;       As with other editors, LSP configurations can become complex. You may need to
;;       install or reinstall the server for your project due to version management quirks
;;       (e.g., asdf or nvm) or other issues.
;;       Fortunately, `lsp-mode` has a great resource site:
;;       https://emacs-lsp.github.io/lsp-mode/
(use-package lsp-mode
  :ensure t
  :straight t
  :defer t
  :hook (;; Replace XXX-mode with concrete major mode (e.g. python-mode)
		 (lsp-mode . lsp-enable-which-key-integration)  ;; Integrate with Which Key
		 ((js-mode                                      ;; Enable LSP for JavaScript
		   tsx-ts-mode                                  ;; Enable LSP for TSX
		   typescript-ts-base-mode                      ;; Enable LSP for TypeScript
		   css-mode                                     ;; Enable LSP for CSS
		   js-ts-mode                                   ;; Enable LSP for JavaScript (TS mode)
		   go-ts-mode                                   ;; Enable LSP for Go
		   json-ts-mode                                 ;; Enable LSP for JSON
		   web-mode) . lsp-deferred))                   ;; Enable LSP for Web (HTML)
  :commands lsp
  :config
  (set-face-attribute 'lsp-face-highlight-textual nil
					  :background "#292e42"
					  :weight 'normal)
  :custom
  (lsp-keymap-prefix "C-c l")                           ;; Set the prefix for LSP commands.
  (lsp-inlay-hint-enable nil)                           ;; Usage of inlay hints.
  (lsp-completion-provider :none)                       ;; Disable the default completion provider.
  (lsp-session-file (locate-user-emacs-file ".lsp-session")) ;; Specify session file location.
  (lsp-log-io nil)                                      ;; Disable IO logging for speed.
  (lsp-idle-delay 0)                                    ;; Set the delay for LSP to 0 (debouncing).
  (lsp-keep-workspace-alive t)                          ;; Keep the workspace alive.
  ;; Core settings
  (lsp-enable-xref t)                                   ;; Enable cross-references.
  (lsp-auto-configure t)                                ;; Automatically configure LSP.
  (lsp-enable-links nil)                                ;; Disable links.
  (lsp-eldoc-enable-hover t)                            ;; Enable ElDoc hover.
  (lsp-enable-file-watchers nil)                        ;; Disable file watchers.
  (lsp-enable-folding nil)                              ;; Disable folding.
  (lsp-enable-imenu t)                                  ;; Enable Imenu support.
  (lsp-enable-indentation nil)                          ;; Disable indentation.
  (lsp-enable-formatting nil)
  (lsp-enable-on-type-formatting nil)                   ;; Disable on-type formatting.
  (lsp-enable-suggest-server-download t)                ;; Enable server download suggestion.
  (lsp-enable-symbol-highlighting t)                    ;; Enable symbol highlighting.
  (lsp-enable-text-document-color t)                    ;; Enable text document color.
  ;; Modeline settings
  (lsp-modeline-code-actions-enable nil)                ;; Keep modeline clean.
  (lsp-modeline-diagnostics-enable nil)                 ;; Use `flycheck' instead.
  (lsp-modeline-workspace-status-enable t)              ;; Display "LSP" in the modeline when enabled.
  (lsp-signature-doc-lines 1)                           ;; Limit echo area to one line.
  (lsp-eldoc-render-all t)                              ;; Render all ElDoc messages.
  ;; Completion settings
  (lsp-completion-enable t)                             ;; Enable completion.
  (lsp-completion-enable-additional-text-edit t)        ;; Enable additional text edits for completions.
  (lsp-enable-snippet nil)                              ;; Disable snippets
  (lsp-completion-show-kind t)                          ;; Show kind in completions.
  ;; Lens settings
  (lsp-lens-enable nil)                                 ;; Disable lens support.
  ;; Semantic settings
  (lsp-semantic-tokens-enable nil)                     ;; Disable semantic tokens.
  :preface
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
	"Try to parse bytecode instead of json."
	(or
	 (when (equal (following-char) ?#)
	   (let ((bytecode (read (current-buffer))))
		 (when (byte-code-function-p bytecode)
		   (funcall bytecode))))
	 (apply old-fn args)))
  (advice-add (if (progn (require 'json)
						 (fboundp 'json-parse-buffer))
				  'json-parse-buffer
				'json-read)
			  :around
			  #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
	"Prepend emacs-lsp-booster command to lsp CMD."
	(let ((orig-result (funcall old-fn cmd test?)))
	  (if (and (not test?)                             ;; for check lsp-server-present?
			   (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
			   lsp-use-plists
			   (not (functionp 'json-rpc-connection))  ;; native json-rpc
			   (executable-find "emacs-lsp-booster"))
		  (progn
			(when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
			  (setcar orig-result command-from-exec-path))
			(message "Using emacs-lsp-booster for %s!" orig-result)
			(cons "emacs-lsp-booster" orig-result))
		orig-result)))

  :init
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  (setq lsp-headerline-breadcrumb-enable nil))





;;; LSP-VTSLS
(use-package lsp-vtsls
  :straight (lsp-vtsls
			 :type git
			 :host github
			 :repo "sdvcrx/lsp-vtsls")
  :after lsp-mode
  :config
  ;; core vtsls settings
  (setq
   lsp-eldoc-render-all t
   lsp-vtsls-server-side-fuzzy-match t
   lsp-vtsls-entries-limit 5000
   lsp-vtsls-auto-use-workspace-tsdk t)

  ;; formatting disabled
  (lsp-register-custom-settings
   '(("typescript.format.enable" nil t)
	 ("javascript.format.enable" nil t)))

  ;; TS/JS preferences
  (let ((preferences
		 (list :includePackageJsonAutoImports "on"
			   :importModuleSpecifier "non-relative"
			   :importModuleSpecifierEnding "js"
			   :useAliasesForRenames nil
			   :autoImportSpecifierExcludeRegexes
			   (vector "^(assert|async_hooks|buffer|child_process|cluster|console|crypto|dgram|dns|domain|events|fs|fs/promises|http|http2|https|inspector|module|net|os|path|path/posix|perf_hooks|process|punycode|querystring|readline|repl|stream|string_decoder|timers|tls|trace_events|tty|url|util|v8|vm|worker_threads|zlib)$"))))
	(lsp-register-custom-settings
	 `(("typescript.preferences" ,preferences t)
	   ("javascript.preferences" ,preferences t)))))

;; TODO: move
(add-hook 'dired-mode-hook #'lsp-dired-mode)


;;; LSP Additional Servers
;; You can extend `lsp-mode' by integrating additional language servers for specific
;; technologies. For example, `lsp-tailwindcss' provides support for Tailwind CSS
;; classes within your HTML files. By using various LSP packages, you can connect
;; multiple LSP servers simultaneously, enhancing your coding experience across
;; different languages and frameworks.

;;; tailwindcss
(use-package lsp-tailwindcss
  :after lsp-mode
  :straight t
  :init

  ;; Validation
  (setq lsp-tailwindcss-validate t)
  (setq lsp-tailwindcss-add-on-mode t)

  ;; Lint rules
  (setq lsp-tailwindcss-lint-css-conflict "error"
		lsp-tailwindcss-lint-invalid-apply "error"
		lsp-tailwindcss-lint-invalid-screen "error"
		lsp-tailwindcss-lint-invalid-variant "error"
		lsp-tailwindcss-lint-invalid-config-path "error"
		lsp-tailwindcss-lint-invalid-tailwind-directive "error"
		lsp-tailwindcss-lint-recommended-variant-order "warning")

  ;; ;; classAttributes
  (setq lsp-tailwindcss-class-attributes
		["class" "className" "style" "classList"])

  ;; ;; classFunctions
  (setq lsp-tailwindcss-class-functions
		["cn" "clsx" "tw" "tw.color" "tw.style"]))


;;; oxlint / oxc_language_server
(defun my/oxlint-server-cmd ()
  "Return the path to the oxlint language server for the current project."
  (let ((server-path (expand-file-name "node_modules/.bin/oxc_language_server"
									   (lsp-workspace-root))))
	(if (file-executable-p server-path)
		server-path
	  (error "Oxc_language_server not found in node_modules/.bin"))))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
	:new-connection (lsp-stdio-connection #'my/oxlint-server-cmd)
	:activation-fn (lsp-activate-on
					"javascript"
					"javascriptreact"
					"typescript"
					"typescriptreact")
	:server-id 'oxlint
	:add-on? t
	:priority -1)))

(defun my/oxlint-fix-all ()
  "Apply fixes to the current buffer using oxlint."
  (interactive)
  (when-let ((client (lsp--find-clients 'oxlint)))
	(lsp-request
	 "workspace/executeCommand"
	 `(:command "oxc.fixAll"
				:arguments [(:uri ,(lsp--buffer-uri))]))))



(provide 'init-lsp)
;;; init-lsp.el ends here
