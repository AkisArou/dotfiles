(deftheme vscode
  "VS Code Dark+ colorscheme for Emacs")

(let (;; UI colors
      (front "#D4D4D4")
      (back "#18191a")

      (tab-current "#1F1F1F")
      (tab-other "#2D2D2D")
      (tab-outside "#252526")

      (left-dark "#252526")
      (left-mid "#373737")
      (left-light "#636369")

      (popup-front "#BBBBBB")
      (popup-back "#202020")
      (popup-highlight-blue "#04395E")
      (popup-highlight-gray "#343B41")

      (split-light "#898989")
      (split-dark "#444444")
      (split-thumb "#424242")

      (cursor-dark-dark "#222222")
      (cursor-dark "#51504F")
      (cursor-light "#AEAFAD")
      (selection "#264F78")
      (line-number "#5A5A5A")

      (diff-red-dark "#4B1818")
      (diff-red-light "#6F1313")
      (diff-red-light-light "#FB0101")
      (diff-green-dark "#373D29")
      (diff-green-light "#4B5632")
      (search-current "#515c6a")
      (search "#613315")

      (git-added "#81b88b")
      (git-modified "#e2c08d")
      (git-deleted "#c74e39")
      (git-renamed "#73c991")
      (git-untracked "#73c991")
      (git-ignored "#8c8c8c")
      (git-conflicting "#e4676b")
      (git-submodule "#8db9e2")

      (context "#404040")
      (context-current "#707070")

      (fold-background "#202d39")

      (suggestion "#6A6A6A")

      ;; Syntax colors
      (gray "#808080")
      (violet "#646695")
      (blue "#569CD6")
      (accent-blue "#4FC1FF")
      (dark-blue "#223E55")
      (medium-blue "#18a2fe")
      (disabled-blue "#729DB3")
      (light-blue "#9CDCFE")
      (green "#6A9955")
      (blue-green "#4EC9B0")
      (light-green "#B5CEA8")
      (red "#F44747")
      (orange "#C48081")
      (light-red "#D16969")
      (yellow-orange "#D7BA7D")
      (yellow "#DCDCAA")
      (dark-yellow "#FFD602")
      (pink "#C586C0")

      (dim-highlight "#51504F"))

  (custom-theme-set-faces
   'vscode

   ;; UI
   `(default ((t :foreground ,front :background ,back)))
   `(cursor ((t :foreground ,back :background ,front)))
   `(mode-line ((t :foreground ,front :background ,left-dark :box nil)))
   `(mode-line-inactive ((t :foreground ,left-light :background ,left-dark :box nil)))
   `(header-line ((t :foreground ,front :background ,left-dark :box nil)))
   `(fringe ((t :foreground ,line-number :background ,back)))
   `(line-number ((t :foreground ,line-number :background ,back)))
   `(line-number-current-line ((t :foreground ,front :background ,back :bold t)))
   `(hl-line ((t :background ,cursor-dark-dark)))
   `(region ((t :background ,selection)))
   `(secondary-selection ((t :background ,selection)))

   ;; Search
   `(isearch ((t :foreground ,front :background ,search-current :bold t)))
   `(isearch-fail ((t :background ,diff-red-light)))
   `(lazy-highlight ((t :background ,search)))

   ;; UI Elements
   `(minibuffer-prompt ((t :foreground ,blue)))
   `(window-divider ((t :foreground ,split-dark)))
   `(vertical-border ((t :foreground ,split-dark)))
   `(widget-field ((t :background ,left-mid)))

   ;; Syntax highlighting
   `(font-lock-comment-face ((t :foreground ,green :slant italic)))
   `(font-lock-string-face ((t :foreground ,orange)))
   `(font-lock-character-face ((t :foreground ,orange)))
   `(font-lock-number-face ((t :foreground ,light-green)))
   `(font-lock-builtin-face ((t :foreground ,blue)))
   `(font-lock-constant-face ((t :foreground ,blue)))
   `(font-lock-keyword-face ((t :foreground ,blue)))
   `(font-lock-type-face ((t :foreground ,blue-green)))
   `(font-lock-function-name-face ((t :foreground ,yellow)))
   `(font-lock-variable-name-face ((t :foreground ,light-blue)))
   `(font-lock-warning-face ((t :foreground ,git-modified)))
   `(font-lock-negation-char-face ((t :foreground ,red)))

   ;; Parenthesis matching
   `(show-paren-match ((t :foreground ,front :background ,cursor-dark :bold t)))
   `(show-paren-mismatch ((t :foreground ,front :background ,red)))

   ;; Diff
   `(diff-added ((t :foreground ,git-added :background ,diff-green-dark)))
   `(diff-removed ((t :foreground ,git-deleted :background ,diff-red-dark)))
   `(diff-context ((t :foreground ,front)))
   `(diff-file-header ((t :foreground ,blue :bold t)))
   `(diff-function ((t :foreground ,yellow)))

   ;; Flycheck/Flymake diagnostics
   `(flymake-error ((t :underline (:style wave :color ,red))))
   `(flymake-warning ((t :underline (:style wave :color ,git-modified))))
   `(flymake-note ((t :underline (:style wave :color ,blue))))

   `(flycheck-error ((t :underline (:style wave :color ,red))))
   `(flycheck-warning ((t :underline (:style wave :color ,git-modified))))
   `(flycheck-info ((t :underline (:style wave :color ,blue))))

   ;; Compilation
   `(compilation-error ((t :foreground ,red)))
   `(compilation-warning ((t :foreground ,git-modified)))
   `(compilation-info ((t :foreground ,blue)))
   `(compilation-line-number ((t :foreground ,line-number)))

   ;; Org mode
   `(org-level-1 ((t :foreground ,blue :bold t :height 1.2)))
   `(org-level-2 ((t :foreground ,blue-green :bold t :height 1.1)))
   `(org-level-3 ((t :foreground ,light-blue :bold t)))
   `(org-level-4 ((t :foreground ,yellow)))
   `(org-level-5 ((t :foreground ,pink)))
   `(org-level-6 ((t :foreground ,orange)))
   `(org-level-7 ((t :foreground ,light-green)))
   `(org-level-8 ((t :foreground ,accent-blue)))
   `(org-code ((t :foreground ,orange)))
   `(org-verbatim ((t :foreground ,orange)))
   `(org-quote ((t :foreground ,gray :slant italic)))
   `(org-block ((t :foreground ,front :background ,cursor-dark-dark)))
   `(org-block-begin-line ((t :foreground ,gray :slant italic)))
   `(org-block-end-line ((t :foreground ,gray :slant italic)))
   `(org-meta-line ((t :foreground ,gray :slant italic)))
   `(org-link ((t :foreground ,accent-blue :underline t)))
   `(org-date ((t :foreground ,accent-blue)))
   `(org-todo ((t :foreground ,git-modified :bold t)))
   `(org-done ((t :foreground ,git-added :bold t)))
   `(org-tag ((t :foreground ,left-light :background ,left-dark)))

   ;; Magit
   `(magit-diff-added ((t :foreground ,git-added :background ,diff-green-dark)))
   `(magit-diff-removed ((t :foreground ,git-deleted :background ,diff-red-dark)))
   `(magit-diff-context ((t :foreground ,front)))
   `(magit-branch-local ((t :foreground ,blue)))
   `(magit-branch-remote ((t :foreground ,git-renamed)))
   `(magit-head ((t :foreground ,blue :bold t)))
   `(magit-tag ((t :foreground ,git-modified)))
   `(magit-section-heading ((t :foreground ,blue :bold t)))
   `(magit-hash ((t :foreground ,line-number)))

   ;; Completion
   `(completions-first-difference ((t :foreground ,blue)))
   `(completions-common-part ((t :foreground ,front)))

   ;; Dired
   `(dired-directory ((t :foreground ,blue)))
   `(dired-symlink ((t :foreground ,accent-blue)))
   `(dired-perm-write ((t :foreground ,git-added)))

   ;; Company (autocomplete)
   `(company-tooltip ((t :foreground ,popup-front :background ,popup-back)))
   `(company-tooltip-selection ((t :foreground ,front :background ,popup-highlight-blue)))
   `(company-tooltip-common ((t :foreground ,blue)))
   `(company-tooltip-annotation ((t :foreground ,left-light)))
   `(company-scrollbar-bg ((t :background ,left-dark)))
   `(company-scrollbar-fg ((t :background ,split-thumb)))

   ;; LSP
   `(lsp-ui-doc-background ((t :background ,popup-back)))
   `(lsp-ui-sideline-global ((t :foreground ,left-light)))
   `(lsp-inlay-hint ((t :foreground ,suggestion :height 0.9)))

   ;; Markdown
   `(markdown-code-face ((t :foreground ,orange :background ,cursor-dark-dark)))
   `(markdown-inline-code-face ((t :foreground ,orange)))
   `(markdown-pre-face ((t :foreground ,orange :background ,cursor-dark-dark)))
   `(markdown-header-face ((t :foreground ,blue :bold t)))
   `(markdown-link-face ((t :foreground ,accent-blue :underline t)))
   `(markdown-url-face ((t :foreground ,accent-blue)))
   `(markdown-bold-face ((t :weight bold)))
   `(markdown-italic-face ((t :slant italic)))

   ;; Rainbow delimiters
   `(rainbow-delimiters-depth-1-face ((t :foreground ,front)))
   `(rainbow-delimiters-depth-2-face ((t :foreground ,pink)))
   `(rainbow-delimiters-depth-3-face ((t :foreground ,blue)))
   `(rainbow-delimiters-depth-4-face ((t :foreground ,yellow)))
   `(rainbow-delimiters-depth-5-face ((t :foreground ,blue-green)))
   `(rainbow-delimiters-depth-6-face ((t :foreground ,orange)))
   `(rainbow-delimiters-depth-7-face ((t :foreground ,light-blue)))
   `(rainbow-delimiters-depth-8-face ((t :foreground ,light-green)))
   `(rainbow-delimiters-unmatched-face ((t :foreground ,red)))

   ;; Eshell
   `(eshell-prompt ((t :foreground ,blue :bold t)))
   `(eshell-ls-directory ((t :foreground ,blue)))
   `(eshell-ls-symlink ((t :foreground ,accent-blue)))
   `(eshell-ls-executable ((t :foreground ,git-added)))

   ;; Info
   `(info-header-node ((t :foreground ,blue :bold t)))
   `(info-header-xref ((t :foreground ,accent-blue :underline t)))
   `(info-menu-header ((t :foreground ,blue :bold t)))
   `(info-node ((t :foreground ,blue :bold t)))
   `(info-reference-item ((t :foreground ,accent-blue)))
   `(info-title-1 ((t :foreground ,blue :bold t :height 1.2)))
   `(info-title-2 ((t :foreground ,blue-green :bold t :height 1.1)))
   `(info-title-3 ((t :foreground ,light-blue :bold t)))
   `(info-title-4 ((t :foreground ,yellow :bold t)))

   ;; Whitespace mode
   `(whitespace-space ((t :foreground ,left-mid)))
   `(whitespace-tab ((t :foreground ,left-mid)))
   `(whitespace-newline ((t :foreground ,left-mid)))
   `(whitespace-trailing ((t :background ,diff-red-dark)))
   `(whitespace-line ((t :background ,cursor-dark-dark)))

   ;; Rainbow identifiers
   `(rainbow-identifiers-identifier-1 ((t :foreground ,blue)))
   `(rainbow-identifiers-identifier-2 ((t :foreground ,blue-green)))
   `(rainbow-identifiers-identifier-3 ((t :foreground ,light-blue)))
   `(rainbow-identifiers-identifier-4 ((t :foreground ,yellow)))
   `(rainbow-identifiers-identifier-5 ((t :foreground ,pink)))
   `(rainbow-identifiers-identifier-6 ((t :foreground ,orange)))
   `(rainbow-identifiers-identifier-7 ((t :foreground ,red)))
   `(rainbow-identifiers-identifier-8 ((t :foreground ,light-green)))
   `(rainbow-identifiers-identifier-9 ((t :foreground ,accent-blue)))

   ;; Undo tree
   `(undo-tree-visualizer-current-face ((t :foreground ,blue)))
   `(undo-tree-visualizer-default-face ((t :foreground ,front)))
   `(undo-tree-visualizer-unmodified-face ((t :foreground ,line-number)))
   `(undo-tree-visualizer-register-face ((t :foreground ,git-modified)))

   ;; Evil (Vim emulation)
   `(evil-cursor ((t :background ,blue)))
   `(evil-ex-substitute-match ((t :background ,search :foreground ,front)))
   `(evil-ex-substitute-replacement ((t :background ,cursor-dark-dark :foreground ,git-added)))
   `(evil-search-highlight-persist-highlight-face ((t :background ,search)))

   ;; Vertico (completion UI)
   `(vertico-current ((t :background ,popup-highlight-blue :foreground ,front :bold t)))
   `(vertico-group-title ((t :foreground ,blue :bold t)))
   `(vertico-group-separator ((t :foreground ,left-light)))

   ;; Corfu (in-buffer completion)
   `(corfu-current ((t :background ,popup-highlight-blue :foreground ,front :bold t)))
   `(corfu-default ((t :background ,popup-back :foreground ,popup-front)))
   `(corfu-border ((t :background ,split-dark)))
   `(corfu-popupchild ((t :background ,popup-back)))

   ;; Orderless (fuzzy completion)
   `(orderless-match-face-0 ((t :foreground ,blue :weight bold)))
   `(orderless-match-face-1 ((t :foreground ,blue-green :weight bold)))
   `(orderless-match-face-2 ((t :foreground ,pink :weight bold)))
   `(orderless-match-face-3 ((t :foreground ,yellow :weight bold)))

   ;; Marginalia (completion annotations)
   `(marginalia-archive ((t :foreground ,left-light)))
   `(marginalia-char ((t :foreground ,pink)))
   `(marginalia-date ((t :foreground ,accent-blue)))
   `(marginalia-directory ((t :foreground ,blue)))
   `(marginalia-documentation ((t :foreground ,gray :slant italic)))
   `(marginalia-file-priv-dir ((t :foreground ,blue)))
   `(marginalia-file-priv-exec ((t :foreground ,git-added)))
   `(marginalia-file-priv-link ((t :foreground ,accent-blue)))
   `(marginalia-file-priv-rare ((t :foreground ,git-modified)))
   `(marginalia-file-priv-read ((t :foreground ,left-light)))
   `(marginalia-file-priv-write ((t :foreground ,git-added)))
   `(marginalia-key ((t :foreground ,blue)))
   `(marginalia-mode ((t :foreground ,left-light)))
   `(marginalia-modified ((t :foreground ,git-modified)))
   `(marginalia-size ((t :foreground ,left-light)))
   `(marginalia-string ((t :foreground ,orange)))
   `(marginalia-symbol ((t :foreground ,pink)))
   `(marginalia-type ((t :foreground ,blue-green)))

   ;; Consult
   `(consult-help ((t :foreground ,left-light)))
   `(consult-highlight-match ((t :foreground ,blue :bold t)))
   `(consult-preview-cursor ((t :background ,blue :foreground ,back)))
   `(consult-preview-line ((t :background ,popup-highlight-blue)))

   ;; Embark
   `(embark-selected ((t :background ,popup-highlight-blue)))
   `(embark-target ((t :foreground ,blue :underline t)))

   ;; Nerd Icons
   `(nerd-icons-dsilver ((t :foreground ,front)))
   `(nerd-icons-dgreen ((t :foreground ,git-added)))
   `(nerd-icons-dblue ((t :foreground ,blue)))
   `(nerd-icons-dpurple ((t :foreground ,pink)))
   `(nerd-icons-dmaroon ((t :foreground ,light-red)))
   `(nerd-icons-dcyan ((t :foreground ,blue-green)))
   `(nerd-icons-orange ((t :foreground ,orange)))

   ;; Elfeed (RSS reader)
   `(elfeed-search-feed-face ((t :foreground ,blue)))
   `(elfeed-search-tag-face ((t :foreground ,left-light)))
   `(elfeed-search-date-face ((t :foreground ,left-light)))
   `(elfeed-search-title-face ((t :foreground ,front)))
   `(elfeed-search-unread-count-face ((t :foreground ,git-modified)))
   `(elfeed-log-error-level-face ((t :foreground ,red)))
   `(elfeed-log-warn-level-face ((t :foreground ,git-modified)))
   `(elfeed-log-info-level-face ((t :foreground ,blue)))

   ;; Notmuch (email)
   `(notmuch-search-count ((t :foreground ,left-light)))
   `(notmuch-search-date ((t :foreground ,left-light)))
   `(notmuch-search-matching-authors ((t :foreground ,front)))
   `(notmuch-search-subject ((t :foreground ,front)))
   `(notmuch-search-unread-face ((t :weight bold)))
   `(notmuch-message-summary-face ((t :foreground ,front :background ,left-dark)))
   `(notmuch-tag-face ((t :foreground ,left-light)))
   `(notmuch-tag-unread ((t :foreground ,git-modified :bold t)))
   `(notmuch-crypto-signature-good ((t :foreground ,git-added)))
   `(notmuch-crypto-signature-bad ((t :foreground ,red)))

   ;; Doom Modeline
   `(doom-modeline-bar ((t :background ,blue)))
   `(doom-modeline-battery-critical ((t :foreground ,red)))
   `(doom-modeline-battery-warning ((t :foreground ,git-modified)))
   `(doom-modeline-battery-normal ((t :foreground ,git-added)))
   `(doom-modeline-evil-normal-state ((t :foreground ,blue)))
   `(doom-modeline-evil-insert-state ((t :foreground ,git-added)))
   `(doom-modeline-evil-visual-state ((t :foreground ,git-modified)))
   `(doom-modeline-evil-operator-state ((t :foreground ,orange)))
   `(doom-modeline-evil-motion-state ((t :foreground ,pink)))
   `(doom-modeline-evil-emacs-state ((t :foreground ,blue-green)))
   `(doom-modeline-evil-replace-state ((t :foreground ,red)))
   `(doom-modeline-info ((t :foreground ,blue)))
   `(doom-modeline-warning ((t :foreground ,git-modified)))
   `(doom-modeline-error ((t :foreground ,red)))
   `(doom-modeline-urgent ((t :foreground ,red)))
   `(doom-modeline-notification ((t :foreground ,blue)))
   `(doom-modeline-buffer-modified ((t :foreground ,git-modified :bold t)))
   `(doom-modeline-project-parent-dir ((t :foreground ,blue)))
   `(doom-modeline-project-dir ((t :foreground ,blue)))
   `(doom-modeline-lsp ((t :foreground ,git-added)))
   `(doom-modeline-lsp-error ((t :foreground ,red)))
   `(doom-modeline-lsp-warning ((t :foreground ,git-modified)))
   `(doom-modeline-debug ((t :foreground ,pink)))
   `(doom-modeline-debug-visual ((t :foreground ,pink)))
   `(doom-modeline-highlight ((t :background ,blue :foreground ,back)))
   `(doom-modeline-icon ((t :foreground ,blue)))

   ;; Treesitter
   `(tree-sitter-hl-face:function ((t :foreground ,yellow)))
   `(tree-sitter-hl-face:method ((t :foreground ,yellow)))
   `(tree-sitter-hl-face:function.call ((t :foreground ,yellow)))
   `(tree-sitter-hl-face:type ((t :foreground ,blue-green)))
   `(tree-sitter-hl-face:type.builtin ((t :foreground ,blue)))
   `(tree-sitter-hl-face:property ((t :foreground ,light-blue)))
   `(tree-sitter-hl-face:variable.parameter ((t :foreground ,light-blue)))
   `(tree-sitter-hl-face:comment ((t :foreground ,green :slant italic)))
   `(tree-sitter-hl-face:string ((t :foreground ,orange)))
   `(tree-sitter-hl-face:number ((t :foreground ,light-green)))
   `(tree-sitter-hl-face:keyword ((t :foreground ,blue)))
   `(tree-sitter-hl-face:operator ((t :foreground ,front)))
   `(tree-sitter-hl-face:variable ((t :foreground ,light-blue)))

   ;; Forge (GitHub/GitLab integration)
   `(forge-post-author ((t :foreground ,blue :bold t)))
   `(forge-post-date ((t :foreground ,left-light)))
   `(forge-issue-label ((t :foreground ,left-light :background ,left-dark)))
   `(forge-issue-milestone-open ((t :foreground ,git-added)))
   `(forge-issue-milestone-closed ((t :foreground ,left-light)))
   `(forge-pullreq-branch ((t :foreground ,accent-blue)))
   `(forge-pullreq-state-open ((t :foreground ,git-added)))
   `(forge-pullreq-state-merged ((t :foreground ,pink)))
   `(forge-pullreq-state-draft ((t :foreground ,left-light)))
   `(forge-pullreq-state-closed ((t :foreground ,red)))

   ;; Dape (debugger)
   `(dape-breakpoint ((t :background ,diff-red-light :foreground ,front)))
   `(dape-breakpoint-disabled ((t :background ,left-mid :foreground ,front)))
   `(dape-fringe-breakpoint ((t :foreground ,red)))
   `(dape-fringe-breakpoint-disabled ((t :foreground ,left-light)))

   ;; Posframe (popover frame)
   `(posframe-border ((t :background ,split-dark)))

   ;; Which-key (keybinding help)
   `(which-key-group-description-face ((t :foreground ,front)))
   `(which-key-command-description-face ((t :foreground ,front)))
   `(which-key-local-map-description-face ((t :foreground ,blue)))
   `(which-key-separator-face ((t :foreground ,left-light)))
   `(which-key-note-face ((t :foreground ,left-light :slant italic)))
   `(which-key-postfix-title-face ((t :foreground ,left-light)))
   `(which-key-highlighted-command-face ((t :foreground ,git-added :underline t)))
   `(which-key-key-face ((t :foreground ,blue :bold t)))

   ;; Pulse (highlight changes)
   `(pulse-highlight-face ((t :background ,cursor-dark-dark)))
   `(pulse-highlight-start-face ((t :background ,context)))
   ))

(custom-theme-set-variables
 'vscode)

(provide-theme 'vscode)
