(defun ek/use-wl-clipboard-p ()
  "Return non-nil if running under Wayland with wl-copy available."
  (and (not (display-graphic-p))
       (getenv "WAYLAND_DISPLAY")
       (executable-find "wl-copy")
       (executable-find "wl-paste")))

(defun ek/use-xsel-p ()
  "Return non-nil if running under X11 with xsel available."
  (and (not (display-graphic-p))
       (getenv "DISPLAY")
       (executable-find "xsel")))

(cond
 ;; Wayland clipboard
 ((ek/use-wl-clipboard-p)
  (message "Using Wayland clipboard via wl-copy/wl-paste")
  (setq interprogram-cut-function
        (lambda (text &optional _)
          (let ((proc (make-process :name "wl-copy"
                                    :buffer nil
                                    :command '("wl-copy" "-f" "-n")
                                    :connection-type 'pipe)))
            (process-send-string proc text)
            (process-send-eof proc))))
  (setq interprogram-paste-function
        (lambda ()
          (shell-command-to-string "wl-paste -n"))))

 ;; X11 clipboard
 ((ek/use-xsel-p)
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
