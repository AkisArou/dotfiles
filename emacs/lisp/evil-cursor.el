;;; evil-cursor.el --- Terminal cursor shapes for Evil states -*- lexical-binding: t; -*-

(defun ek/terminal-cursor-update ()
  "Set terminal cursor shape depending on Evil state."
  (when (not (display-graphic-p))
	(cond
	 ((evil-insert-state-p)
	  ;; vertical bar
	  (send-string-to-terminal "\e[6 q"))
	 ((evil-visual-state-p)
	  ;; block
	  (send-string-to-terminal "\e[2 q"))
	 ((evil-emacs-state-p)
	  ;; horizontal bar
	  (send-string-to-terminal "\e[5 q"))
	 (t
	  ;; normal, motion, replace, etc.: block
	  (send-string-to-terminal "\e[2 q")))))

;; Hook to update cursor after each command
(add-hook 'post-command-hook 'ek/terminal-cursor-update)

(provide 'evil-cursor)
;;; evil-terminal-cursor.el ends here
