;; Clipboard setup for Wayland and X11
(cond
 ;; Wayland
 ((and (string= (getenv "XDG_SESSION_TYPE") "wayland")
       (executable-find "wl-copy")
       (executable-find "wl-paste"))
  (message "Using Wayland clipboard via wl-copy/wl-paste")

  (defun my-wl-copy (text)
    "Copy TEXT using wl-copy in terminal, otherwise fallback to GUI."
    (if (display-graphic-p)
        (gui-select-text text)
      (let ((proc (make-process :name "wl-copy"
                                :buffer nil
                                :command '("wl-copy")
                                :connection-type 'pipe)))
        (process-send-string proc text)
        (process-send-eof proc))))

  (defun my-wl-paste ()
    "Paste using wl-paste in terminal, otherwise fallback to GUI."
    (if (display-graphic-p)
        (gui-selection-value)
      (shell-command-to-string "wl-paste --no-newline")))

  (setq interprogram-cut-function #'my-wl-copy)
  (setq interprogram-paste-function #'my-wl-paste))

 ;; X11
 ((and (fboundp 'ek/use-xsel-p)
       (ek/use-xsel-p))
  (message "Using X11 clipboard via xsel")
  (setq interprogram-cut-function
        (lambda (text &optional _)
          (let ((proc (make-process :name "xsel"
                                    :buffer nil
                                    :command '("xsel" "--clipboard" "--input")
                                    :connection-type 'pipe)))
            (process-send-string proc text)
            (process-send-eof proc))))
  (setq interprogram-paste-function
        (lambda ()
          (shell-command-to-string "xsel --clipboard --output")))))

(provide 'clipboard)
