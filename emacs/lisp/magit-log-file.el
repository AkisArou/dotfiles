;;; git-file-history.el --- Browse git history with consult and preview

(require 'consult)
(require 'magit)
(require 'vc-git)

(defun my/git-file-history-candidates (file)
  "Get git log entries for FILE as consult candidates."
  (let* ((default-directory (vc-git-root file))
		 (relative-file (file-relative-name file default-directory))
		 (log-output (shell-command-to-string
					  (format "git log --pretty=format:'%%H\t%%ai\t%%an\t%%s' -- %s"
							  (shell-quote-argument relative-file)))))
	(when (string-empty-p log-output)
	  (user-error "No git history found for %s" file))
	(mapcar (lambda (line)
			  (let* ((parts (split-string line "\t"))
					 (hash (nth 0 parts))
					 (date (nth 1 parts))
					 (author (nth 2 parts))
					 (subject (mapconcat 'identity (nthcdr 3 parts) "\t"))
					 (short-hash (substring hash 0 7))
					 (display (format "%-8s  %-19s  %-20s  %s"
									  short-hash
									  date
									  (truncate-string-to-width author 20 nil nil t)
									  subject)))
				(cons display hash)))
			(split-string log-output "\n" t))))

(defun my/git-file-at-commit (file commit)
  "Show FILE contents at COMMIT in a buffer with syntax highlighting."
  (let* ((default-directory (vc-git-root file))
		 (relative-file (file-relative-name file default-directory))
		 (buffer-name (format "*%s @ %s*"
							  (file-name-nondirectory file)
							  (substring commit 0 7)))
		 (content (shell-command-to-string
				   (format "git show %s:%s"
						   (shell-quote-argument commit)
						   (shell-quote-argument relative-file))))
		 (buf (get-buffer-create buffer-name)))
	(with-current-buffer buf
	  (let ((inhibit-read-only t))
		(erase-buffer)
		(insert content)
		;; Set the major mode based on file extension for syntax highlighting
		(let ((buffer-file-name file))
		  (set-auto-mode))
		(view-mode 1)
		(goto-char (point-min))))
	buf))

(defvar my/git-file-history--current-file nil
  "Current file being browsed in git history.")

(defun my/git-file-history-state ()
  "Create a state function for consult preview."
  (let ((preview-buffer nil)
		(original-window (selected-window)))
	(lambda (action cand)
	  (pcase action
		('preview
		 (when cand
		   (let ((commit (if (consp cand) (cdr cand) cand)))
			 (setq preview-buffer
				   (my/git-file-at-commit my/git-file-history--current-file commit))
			 ;; Show in the main window (not minibuffer window)
			 (with-selected-window (or (window-in-direction 'above (minibuffer-window))
									   (get-largest-window))
			   (switch-to-buffer preview-buffer)))))
		('return
		 cand)
		('exit
		 nil)))))

;;;###autoload
(defun my/git-file-history ()
  "Browse git history of current file with preview in main window."
  (interactive)
  (unless buffer-file-name
	(user-error "Buffer is not visiting a file"))
  (unless (vc-git-root buffer-file-name)
	(user-error "File is not in a git repository"))

  (setq my/git-file-history--current-file buffer-file-name)

  (let* ((candidates (my/git-file-history-candidates buffer-file-name))
		 (selected (consult--read
					candidates
					:prompt "Git history: "
					:lookup #'consult--lookup-cdr
					:sort nil
					:require-match t
					:category 'git-commit
					:history 'my/git-file-history--history
					:state (my/git-file-history-state))))
	(when selected
	  (let ((buf (my/git-file-at-commit my/git-file-history--current-file selected)))
		;; Show in the current window, not a new one
		(switch-to-buffer buf)
		(message "Showing %s at commit %s"
				 (file-name-nondirectory buffer-file-name)
				 (substring selected 0 7))))))

(provide 'git-file-history)
;;; git-file-history.el ends here
