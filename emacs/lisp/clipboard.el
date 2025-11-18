;; Detect display server (Wayland vs Xorg)
(defun system-wayland-p ()
  "Return non-nil if running under Wayland."
  (and (getenv "WAYLAND_DISPLAY")
       (not (string-empty-p (getenv "WAYLAND_DISPLAY")))))

(defun system-xorg-p ()
  "Return non-nil if running under Xorg."
  (and (getenv "DISPLAY")
       (not (string-empty-p (getenv "DISPLAY")))))

;; -----------------------------
;; Wayland clipboard functions
;; -----------------------------
(setq wl-copy-process nil)

(defun wl-copy (text)
  (setq wl-copy-process
        (make-process :name "wl-copy"
                      :buffer nil
                      :command '("wl-copy" "-f" "-n")
                      :connection-type 'pipe
                      :noquery t))
  (process-send-string wl-copy-process text)
  (process-send-eof wl-copy-process))

(defun wl-paste ()
  (if (and wl-copy-process (process-live-p wl-copy-process))
      nil ;; return nil if we're the current clipboard owner
    (shell-command-to-string "wl-paste -n | tr -d '\r'")))

;; --------------------------------
;; Xorg clipboard using xsel
;; --------------------------------
(defun xsel-copy (text)
  (let ((process-connection-type nil))
    (let ((proc (start-process "xsel-copy" nil "xsel" "--clipboard" "--input")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun xsel-paste ()
  (shell-command-to-string "xsel --clipboard --output"))

;; ------------------------------
;; Select backend automatically
;; ------------------------------
(cond
 ((system-wayland-p)
  (setq interprogram-cut-function #'wl-copy
        interprogram-paste-function #'wl-paste))

 ((system-xorg-p)
  (setq interprogram-cut-function #'xsel-copy
        interprogram-paste-function #'xsel-paste))

 (t
  (message "Warning: Unknown display server; clipboard integration disabled.")))

(provide 'clipboard)
