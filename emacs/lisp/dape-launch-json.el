;;; dape-launch-json.el --- Import VS Code launch.json into dape  -*- lexical-binding: t; -*-

;; This file provides a one-shot translator from .vscode/launch.json
;; to Emacs dape configurations. It is intentionally explicit and
;; conservative, in line with dape's philosophy.

(require 'json)
(require 'project)
(require 'cl-lib)

;;;; Project helpers

(defun dape-lj--project-root ()
  "Return the current project root or signal an error."
  (let ((proj (project-current)))
	(unless proj
	  (error "dape-launch-json: not inside a project"))
	(project-root proj)))

(defun dape-lj--find-launch-json ()
  "Return absolute path to .vscode/launch.json in the project."
  (let ((file (expand-file-name ".vscode/launch.json"
								(dape-lj--project-root))))
	(unless (file-exists-p file)
	  (error "dape-launch-json: no .vscode/launch.json found"))
	file))

;;;; Variable resolution

(defun dape-lj--resolve-vars (value)
  "Resolve a minimal subset of VS Code variables in VALUE."
  (cond
   ((stringp value)
	(replace-regexp-in-string
	 "\\${workspaceFolder}"
	 (directory-file-name (dape-lj--project-root))
	 value))
   (t value)))

;;;; Adapter mapping

(defun dape-lj--translate-type (type)
  "Map VS Code adapter TYPE to a dape adapter type."
  (pcase type
	("chrome" "pwa-chrome")
	("pwa-chrome" "pwa-chrome")
	("node" "pwa-node")
	("reactnativedirect" "pwa-node")
	(_ (error "dape-launch-json: unsupported adapter type %S" type))))

;;;; JSON helpers

(defun dape-lj--json-object-to-plist (obj)
  "Convert JSON object OBJ (alist) to a plist, resolving variables."
  (cl-loop for (k . v) in obj
		   append (list
				   (intern (format ":%s" k))
				   (dape-lj--resolve-vars v))))

(defun dape-lj--translate-config (cfg)
  "Translate a single VS Code configuration CFG into a dape entry."
  (let* ((name    (alist-get 'name cfg))
		 (type    (alist-get 'type cfg))
		 (request (alist-get 'request cfg))
		 (plist   (dape-lj--json-object-to-plist cfg)))
	`(,name
	  :type ,(dape-lj--translate-type type)
	  :request ,request
	  ,@(cl-loop for (k v) on plist by #'cddr
				 unless (memq k '(:name :type :request))
				 collect k
				 and collect v))))

;;;; Public command

;;;###autoload
(defun dape-import-launch-json ()
  "Translate .vscode/launch.json into dape-configs.

The result is written to a review buffer; nothing is applied
automatically."
  (interactive)
  (let* ((json-object-type 'alist)
		 (json-array-type  'list)
		 (json-key-type    'symbol)
		 (file   (dape-lj--find-launch-json))
		 (data   (json-read-file file))
		 (configs (alist-get 'configurations data))
		 (translated (mapcar #'dape-lj--translate-config configs)))
	(with-current-buffer (get-buffer-create "*dape launch.json*")
	  (erase-buffer)
	  (emacs-lisp-mode)
	  (insert ";; Generated from .vscode/launch.json\n")
	  (insert ";; Review before pasting into your config or .dir-locals.el\n\n")
	  (insert "(setq dape-configs\n      '(\n")
	  (dolist (cfg translated)
		(pp cfg (current-buffer))
		(insert "\n"))
	  (insert "))\n")
	  (goto-char (point-min))
	  (pop-to-buffer (current-buffer)))))

(provide 'dape-launch-json)

;;; dape-launch-json.el ends here
