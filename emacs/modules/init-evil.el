;;; init-evil.el --- Evil Setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Evil configuration

;;; Code:

;; EVIL
;; The `evil' package provides Vim emulation within Emacs, allowing
;; users to edit text in a modal way, similar to how Vim
;; operates. This setup configures `evil-mode' to enhance the editing
;; experience.
(use-package evil
  :ensure t
  :straight t
  :defer t
  :hook
  (after-init . evil-mode)
  :init
  (setq evil-want-integration t)      ;; Integrate `evil' with other Emacs features (optional as it's true by default).
  (setq evil-want-keybinding nil)     ;; Disable default keybinding to set custom ones.
  (setq evil-want-C-u-scroll t)       ;; Makes C-u scroll
  (setq evil-symbol-word-search t)
  (setq evil-search-module 'evil-search)

  :config
  (setq evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'box)

  ;; PERF: Stop copying the selection to the clipboard each time the cursor
  ;; moves in visual mode. Why? Because on most non-X systems (and in terminals
  ;; with clipboard plugins like xclip.el active), Emacs will spin up a new
  ;; process to communicate with the clipboard for each movement. On Windows,
  ;; older versions of macOS (pre-vfork), and Waylang (without pgtk), this is
  ;; super expensive and can lead to freezing and/or zombie processes.
  ;;
  ;; UX: It also clobbers clipboard managers (see emacs-evil/evil#336).
  (setq evil-visual-update-x-selection-p nil)

  (evil-set-undo-system 'undo-tree)   ;; Uses the undo-tree package as the default undo system

  (define-key evil-motion-state-map (kbd "C-z") nil)
  (define-key evil-insert-state-map (kbd "C-e") nil)
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)

  ;; Set the leader key to space for easier access to custom commands. (setq evil-want-leader t)
  (setq evil-leader/in-all-states t)  ;; Make the leader key available in all states.
  (setq evil-want-fine-undo nil)      ;; Default but keep it for reference

  (define-key evil-insert-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-c") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-s") 'evil-write-all)

  ;; Define the leader key as Space
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

  (evil-define-key 'normal 'global (kbd "<leader> h") 'evil-ex-nohighlight)

  (evil-define-key 'normal 'global (kbd "<leader> r") (lambda()
                                                        (interactive)
                                                        (elfeed-update)
                                                        (elfeed)))

  ;; Keybindings for searching and finding files.
  (evil-define-key 'normal 'global (kbd "<leader> f f") 'project-find-file)
  (evil-define-key 'normal 'global (kbd "<leader> f s") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader> f S") 'consult-gitgrep)
  (evil-define-key 'normal 'global (kbd "<leader> f q") 'consult-compile-error)
  (evil-define-key 'normal 'global (kbd "<leader> f h") 'consult-info)
  (evil-define-key 'normal 'global (kbd "<leader> f m") 'consult-man)
  (evil-define-key 'normal 'global (kbd "<leader> f l") 'vertico-repeat)
  (evil-define-key 'normal 'global (kbd "<leader> /") 'consult-line)

  ;; Flycheck navigation
  (with-eval-after-load 'evil
    (define-key evil-window-map (kbd "C-d") #'flycheck-posframe-display-errors-manually))

  (evil-define-key 'normal 'global (kbd "<leader> f d") 'consult-flycheck)

  (defun my-show-flycheck-posframe-after (&rest _)
    "Show flycheck posframe after navigation."
    (run-with-idle-timer 0 nil #'flycheck-posframe-display-errors-manually))

  (defun my/flycheck-next-error-of-severity (severity)
    "Go to the next Flycheck error of SEVERITY, wrapping to the beginning if needed."
    (interactive)
    (my-show-flycheck-posframe-after)
    (let* ((errors (seq-filter (lambda (e) (eq (flycheck-error-level e) severity))
                               (flycheck-overlay-errors-in (point-min) (point-max))))
           (next (seq-find (lambda (e) (> (flycheck-error-pos e) (point))) errors)))
      (if next
          (goto-char (flycheck-error-pos next))
        ;; Wrap to the first error
        (if errors
            (goto-char (flycheck-error-pos (car errors)))
          (message "No %s errors found" severity)))))

  (defun my/flycheck-previous-error-of-severity (severity)
    "Go to the previous Flycheck error of SEVERITY, wrapping to the end if needed."
    (interactive)
    (my-show-flycheck-posframe-after)
    (let* ((errors (seq-filter (lambda (e) (eq (flycheck-error-level e) severity))
                               (flycheck-overlay-errors-in (point-min) (point-max))))
           (prev (car (last (seq-filter (lambda (e) (< (flycheck-error-pos e) (point))) errors)))))
      (if prev
          (goto-char (flycheck-error-pos prev))
        ;; Wrap to the last error
        (if errors
            (goto-char (flycheck-error-pos (car (last errors))))
          (message "No %s errors found" severity)))))

  ;; Specific wrappers
  (defun my/flycheck-next-error-only () (interactive) (my/flycheck-next-error-of-severity 'error))
  (defun my/flycheck-previous-error-only () (interactive) (my/flycheck-previous-error-of-severity 'error))
  (defun my/flycheck-next-warning-only () (interactive) (my/flycheck-next-error-of-severity 'warning))
  (defun my/flycheck-previous-warning-only () (interactive) (my/flycheck-previous-error-of-severity 'warning))

  ;; Keybindings
  (evil-define-key 'normal 'global (kbd "] e") 'my/flycheck-next-error-only)
  (evil-define-key 'normal 'global (kbd "[ e") 'my/flycheck-previous-error-only)
  (evil-define-key 'normal 'global (kbd "] w") 'my/flycheck-next-warning-only)
  (evil-define-key 'normal 'global (kbd "[ w") 'my/flycheck-previous-warning-only)
  (evil-define-key 'normal 'global (kbd "] d") 'flycheck-next-error)
  (evil-define-key 'normal 'global (kbd "[ d") 'flycheck-previous-error)

  ;; LSP
  (evil-define-key 'normal 'global (kbd "gd") 'lsp-find-definition)
  (evil-define-key 'normal 'global (kbd "grr") 'lsp-find-references)
  (evil-define-key 'normal 'global (kbd "gD") 'lsp-find-declaration)
  (evil-define-key 'normal 'global (kbd "gri") 'lsp-find-implementation)
  (evil-define-key 'normal 'global (kbd "grt") 'lsp-find-type-definition)
  (evil-define-key 'normal 'global (kbd "gra") 'lsp-execute-code-action)
  (evil-define-key 'normal 'global (kbd "grn") 'lsp-rename)

  ;; Notmuch
  (evil-define-key 'normal 'global (kbd "<leader> m") 'notmuch)

  ;; Trigger completion at point in Evil insert mode
  (evil-define-key 'insert global-map (kbd "C-SPC") #'completion-at-point)
  (evil-define-key 'insert global-map (kbd "C-@")   #'completion-at-point)

  ;; Trigger completion at point in all minibuffer maps
  (dolist (map (list minibuffer-local-map
                     minibuffer-local-ns-map
                     minibuffer-local-completion-map
                     minibuffer-local-must-match-map))
    (define-key map (kbd "M-SPC") #'completion-at-point))

  ;; Dired commands for file management
  (evil-define-key 'normal 'global (kbd "<leader> x d") 'dired)
  (evil-define-key 'normal 'global (kbd "<leader> x j") 'dired-jump)
  (evil-define-key 'normal 'global (kbd "<leader> x f") 'find-file)

  ;; Diff-HL navigation for version control
  (evil-define-key 'normal 'global (kbd "] h") 'diff-hl-next-hunk) ;; Next diff hunk
  (evil-define-key 'normal 'global (kbd "[ h") 'diff-hl-pevious-hunk) ;; Previous diff hunk

  ;; File exploration
  (evil-define-key 'normal 'global (kbd "<leader> e") 'dired-jump)

  ;; Magit keybindings for Git integration
  (evil-define-key 'normal 'global (kbd "<leader> g s") 'magit-status)      ;; Open Magit status
  (evil-define-key 'normal 'global (kbd "<leader> g l") 'magit-log-current) ;; Show current log
  (evil-define-key 'normal 'global (kbd "<leader> g f") 'my/git-file-history) ;; Show current log for buffer
  (evil-define-key 'normal 'global (kbd "<leader> g d") 'magit-diff-buffer-file) ;; Show diff for the current file
  (evil-define-key 'normal 'global (kbd "<leader> g D") 'diff-hl-show-hunk) ;; Show diff for a hunk
  (evil-define-key 'normal 'global (kbd "<leader> g b") 'vc-annotate)       ;; Annotate buffer with version control info
  (evil-define-key 'normal 'global (kbd "<leader> g t") 'git-timemachine)       ;; Annotate buffer with version control info
  (evil-define-key 'normal 'global
    (kbd "<leader> g i")
    (lambda ()
      (interactive)
      (consult-gh-issue-list "nablesolutions/nable-solutions")))

  ;; Buffer management keybindings
  (defun my/project-kill-buffers-no-confirm ()
    "Kill all file buffers in the current project, skipping special buffers and LSP.
  Modified buffers are automatically saved before being killed."
    (interactive)
    (let ((project (project-current)))
      (when project
        (dolist (buf (project-buffers project))
          ;; Only handle normal file buffers
          (when (and (buffer-file-name buf)
                     (not (string-match-p "^\\*" (buffer-name buf))))
            (with-current-buffer buf
              ;; Save if modified
              (when (buffer-modified-p)
                (save-buffer))
              ;; Kill buffer
              (kill-buffer buf)))))))

  (defun kill-other-buffers ()
    (interactive)
    (dolist (buf (delq (current-buffer) (buffer-list)))
      (unless (string-prefix-p "*" (buffer-name buf))
        (kill-buffer buf))))

  (evil-define-key 'normal 'global (kbd "] b") 'switch-to-next-buffer) ;; Switch to next buffer
  (evil-define-key 'normal 'global (kbd "[ b") 'switch-to-prev-buffer) ;; Switch to previous buffer
  (evil-define-key 'normal 'global (kbd "<leader> b b") 'ibuffer) ;; Open Ibuffer
  (evil-define-key 'normal 'global (kbd "<leader> b d") 'kill-current-buffer) ;; Kill current buffer
  (evil-define-key 'normal 'global (kbd "<leader> b a") 'my/project-kill-buffers-no-confirm) ;; Kill project buffers
  (evil-define-key 'normal 'global (kbd "<leader> b o") #'kill-other-buffers) ;; Kill project buffers
  (evil-define-key 'normal 'global (kbd "<leader> b s") 'save-buffer) ;; Save buffer

  ;; Project management keybindings
  (defun get-project-root ()
    (when (fboundp 'projectile-project-root)
      (projectile-project-root)))

  ;; Ripgrep the current word from project root
  (defun consult-ripgrep-region-or-word ()
    "Run `consult-ripgrep` on selected region in visual mode, or word at point otherwise."
    (interactive)
    (let ((search-text (if (use-region-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (thing-at-point 'word t))))
      ;; Deactivate the region so it doesn't interfere with the search
      (deactivate-mark)
      (consult-ripgrep (get-project-root) search-text)))

  (evil-define-key 'normal 'global (kbd "<leader> f e") 'consult-project-buffer) ;; Consult buffers
  (evil-define-key 'normal global-map (kbd "<leader> f w") #'consult-ripgrep-region-or-word)
  (evil-define-key 'visual global-map (kbd "<leader> f w") #'consult-ripgrep-region-or-word)
  (evil-define-key 'normal 'global (kbd "<leader> p p") 'project-switch-project) ;; Switch project
  (evil-define-key 'normal 'global (kbd "<leader> p D") 'project-dired) ;; Dired for project

  ;; Yank from kill ring
  ;; (evil-define-key 'normal 'global (kbd "P") 'consult-yank-from-kill-ring)
  (evil-define-key 'normal 'global (kbd "<leader> P") 'consult-yank-from-kill-ring)

  ;; Embark actions for contextual commands
  (evil-define-key 'normal 'global (kbd "<leader> .") 'embark-act)

  ;; Undo tree visualization
  (evil-define-key 'normal 'global (kbd "<leader> u") 'undo-tree-visualize)

  ;; Help keybindings
  ;; (evil-define-key 'normal 'global (kbd "<leader> h m") 'describe-mode) ;; Describe current mode
  ;; (evil-define-key 'normal 'global (kbd "<leader> h f") 'describe-function) ;; Describe function
  ;; (evil-define-key 'normal 'global (kbd "<leader> h v") 'describe-variable) ;; Describe variable
  ;; (evil-define-key 'normal 'global (kbd "<leader> h k") 'describe-key) ;; Describe key


  ;; Tab navigation
  (evil-define-key 'normal 'global (kbd "] t") 'tab-next) ;; Go to next tab
  (evil-define-key 'normal 'global (kbd "[ t") 'tab-previous) ;; Go to previous tab


  ;; Custom example. Formatting with prettier tool.
  (evil-define-key 'normal 'global (kbd "<leader> b f")
    (lambda ()
      (interactive)
      (shell-command (concat "prettier --write " (shell-quote-argument (buffer-file-name))))
      (revert-buffer t t t)))


  ;; Text-objects
  (defun my-evil-find-nearest-quote ()
    "Find the nearest quote character."
    (let ((point (point))
          (quotes '(?\" ?\' ?\`))
          (nearest nil)
          (min-dist most-positive-fixnum))
      (dolist (q quotes)
        (save-excursion
          (let ((pos (search-backward (char-to-string q) nil t)))
            (when (and pos (< (- point pos) min-dist))
              (setq min-dist (- point pos))
              (setq nearest q)))))
      nearest))

  (evil-define-text-object evil-inner-any-quote (count &optional beg end type)
    (let ((quote (my-evil-find-nearest-quote)))
      (when quote
        (evil-select-quote quote beg end type count nil))))

  (evil-define-text-object evil-a-any-quote (count &optional beg end type)
    (let ((quote (my-evil-find-nearest-quote)))
      (when quote
        (evil-select-quote quote beg end type count t))))

  (define-key evil-inner-text-objects-map "q" 'evil-inner-any-quote)
  (define-key evil-outer-text-objects-map "q" 'evil-a-any-quote)

  ;; Enable evil mode
  (evil-mode 1))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "K") #'eldoc-box-help-at-point))

;; Ensure `evil-shift-width' always matches `tab-width'; evil does not police
;; this itself, so we must. Except in org-mode, where `tab-width' *must*
;; default to 8, which isn't a sensible default for `evil-shift-width'.
(add-hook 'after-change-major-mode-hook
          (defun +evil-adjust-shift-width-h ()
            (unless (derived-mode-p 'org-mode)
              (setq-local evil-shift-width tab-width))))


;;; EVIL-COMMENTARY
(use-package evil-commentary
  :after evil
  :straight t
  :config
  (evil-commentary-mode 1))

;;; EVIL-NUMBERS
(use-package evil-numbers
  :after evil
  :straight t
  :config
  (evil-define-key '(normal visual) 'global
    (kbd "C-a") 'evil-numbers/inc-at-pt
    (kbd "C-x") 'evil-numbers/dec-at-pt
    (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental
    (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental))

;;; EMACS-TMUX-NAVIGATOR
(use-package emacs-tmux-navigator
  :after evil
  :config
  (emacs-tmux-navigator-mode 1))

;; EVIL COLLECTION
;; The `evil-collection' package enhances the integration of
;; `evil-mode' with various built-in and third-party packages. It
;; provides a better modal experience by remapping keybindings and
;; commands to fit the `evil' style.
(use-package evil-collection
  :defer t
  :straight t
  :ensure t
  :custom
  (evil-collection-want-find-usages-bindings nil)
  (evil-collection-want-unimpaired-p nil)
  ;; Hook to initialize `evil-collection' when `evil-mode' is activated.
  :hook
  (evil-mode . evil-collection-init))


(use-package evil-cursor
  :after evil)

;; EVIL SURROUND
;; The `evil-surround' package provides text object surround
;; functionality for `evil-mode'. This allows for easily adding,
;; changing, or deleting surrounding characters such as parentheses,
;; quotes, and more.
;;
;; With this you can change 'hello there' with ci'" to have
;; "hello there" and cs"<p> to get <p>hello there</p>.
;; More examples here:
;; - https://github.com/emacs-evil/evil-surround?tab=readme-ov-file#examples
(use-package evil-surround
  :ensure t
  :straight t
  :after evil-collection
  :config
  (global-evil-surround-mode 1))

;; EVIL-TEXTOBJ-TREE-SITTER
(use-package evil-textobj-tree-sitter
  :after evil-collection
  :ensure t
  :config
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner")))

;; EVIL-ANZU
(use-package evil-anzu
  :after evil
  :ensure t
  :init
  (global-anzu-mode 1))


;; EVIL-ANZU
(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1)

  (define-key evil-normal-state-map (kbd "C-n") #'evil-mc-make-and-goto-next-match)
  (define-key evil-visual-state-map (kbd "C-n") #'evil-mc-make-and-goto-next-match)
  (define-key evil-normal-state-map (kbd "C-p") #'evil-mc-make-and-goto-prev-match)
  (define-key evil-visual-state-map (kbd "C-p") #'evil-mc-make-and-goto-prev-match)
  (define-key evil-normal-state-map (kbd "C-x") #'evil-mc-skip-and-goto-next-match)
  (define-key evil-visual-state-map (kbd "C-x") #'evil-mc-skip-and-goto-next-match)
  (define-key evil-normal-state-map [escape] #'evil-mc-undo-all-cursors))


;;; HIGHLIGHT VIM OP
(defface my/flash-face
  '((t (:background "#7b5fbf" :extend t)))
  "Face used to flash evil operator ranges.")

(defun my/flash-region (beg end)
  "Flash the region from BEG to END using a temporary overlay."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'my/flash-face)
    (run-at-time 0.08 nil #'delete-overlay ov)))

(defun my/evil-flash-motion (orig beg end &rest args)
  "Flash region affected by evil operator."
  (let ((result (apply orig beg end args)))
    ;; absolutely ensure region is not active
    (deactivate-mark)
    ;; flash without leaving any highlight behind
    (my/flash-region beg end)
    result))

;; Advice evil operators that use (beg end)
(dolist (op '(evil-yank
              evil-yank-line))
  (advice-add op :around #'my/evil-flash-motion))


;; Minibuffer completion helpers (after all modules are loaded)
(defun my/minibuffer-setup-dabbrev ()
  "Setup dabbrev completion in minibuffer for evil search."
  (when (eq this-command 'evil-ex-search-forward)
    (setq-local completion-at-point-functions
                (list #'cape-dabbrev))))

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-dabbrev)

;; Enable corfu in minibuffer
(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in minibuffer."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)




(provide 'init-evil)
;;; init-evil.el ends here
