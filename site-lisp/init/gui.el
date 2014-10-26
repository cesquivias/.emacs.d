;; Functions

(defun max-frame-rows (&optional frame)
  "The number of rows for the frame and still fit on the selected screen."
  (/ (- (x-display-pixel-height frame) 70)
     (/ (frame-pixel-height frame) (frame-height frame))))

(defun set-frame-double-width ()
  "Doubles the width of the selected frame.
Adds some extra width for scrollbars"
  (interactive)
  (let ((f (selected-frame)))
    (set-frame-width f (+ (* 2 (frame-width f)) 4))))

(defun double-frame-right ()
  "Doubles the width of the frame and splits the window."
  (interactive)
  (set-frame-double-width)
  (split-window-right))


;;;; INIT

;; Is emacs running on a window-system? This is tricky 'cause
;; window-system returns nil when started from daemon. Have to do some
;; tricky logic to determine if we'll have frames at any time
(setq on-x-windows?
      (or (functionp 'x-initialize-window-system)
          (eq (window-system) 'x))) ;; daemon, xwin startup

;; (setq -ns-initialized
;;       (or (and (boundp '-ns-initialized)
;;                -ns-initialized)
;;           (if (functionp 'ns-initialize-window-system)
;;               (ns-initialize-window-system))))

(setq on-window-system?
      (or (window-system) ;; non-daemon, GUI startup
          ns-initialized ;; daemon, mac startup
          on-x-windows?))
(setq daemon-mode? (and on-window-system? (not (window-system))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode 'right)

;; (if (font-info "Liberation Mono")
;;     (set-face-attribute 'default nil :font "Liberation Mono-10"))

;; Adjust GUI window position
(let* ((top (if on-x-windows? 23 0)) ;; x doesn't compensate for the title bar
       (left (if on-x-windows? 4 0))) ;; x doesn't compensate for the chrome
  (add-to-list 'default-frame-alist `(top . ,top))
  (add-to-list
   'initial-frame-alist `(left . ,left)))
(add-to-list 'default-frame-alist '(width . 80))

(add-to-list 'default-frame-alist '(alpha . 90))

(if daemon-mode?
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (if (not (eq (framep frame) 't))
                    (set-frame-height frame (max-frame-rows frame)))))
    (add-to-list 'initial-frame-alist `(height . ,(max-frame-rows))))

(if ns-initialized
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (when (eq (framep frame) 'ns)
                  (ns-do-applescript "tell application \"Emacs\" to activate")
                  (set-frame-position frame 0 0)))))

(when on-window-system?
  (global-set-key (kbd "C-+")
                  (lambda ()
                    (interactive)
                    (set-frame-height (selected-frame) (max-frame-rows)))))
