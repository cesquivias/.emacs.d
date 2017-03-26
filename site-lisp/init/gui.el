;; Functions

(defun max-frame-rows (&optional frame)
  "The number of rows for the frame and still fit on the selected screen."
  (if (and (= emacs-major-version 24) (< emacs-minor-version 4))
      (/ (- (x-display-pixel-height frame) 70)
         (/ (frame-pixel-height frame) (frame-height frame)))
    (let* ((monitor-attrs (assoc 'workarea (frame-monitor-attributes frame)))
           (monitor-width (nth 4 monitor-attrs)))
      (/ (- monitor-width 70)
         (/ (frame-pixel-height frame) (frame-height frame))))))

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
          (boundp 'ns-initialized) ;; daemon, mac startup
          on-x-windows?))

(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode 'right)

;; Adjust GUI window position
(unless desktop-save-mode
  (if (and (= emacs-major-version 24) (< emacs-minor-version 4))
      (let* ((top (if on-x-windows? 23 0))
             (left (if on-x-windows? 4 0)))
        (add-to-list 'default-frame-alist `(top . ,top))
        (add-to-list
         'initial-frame-alist `(left . ,left)))
    (let* ((monitor-attrs (assoc 'workarea (frame-monitor-attributes)))
           (top (nth 2 monitor-attrs))
           (left (nth 1 monitor-attrs)))
      (add-to-list 'default-frame-alist `(top . ,top))
      (add-to-list
       'initial-frame-alist `(left . ,left))))
  
  (add-to-list 'default-frame-alist '(width . 80)))

(add-to-list 'default-frame-alist '(alpha . 90))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (if (not (eq (framep frame) 't))
                    (set-frame-height frame (max-frame-rows frame)))))
    (add-to-list 'initial-frame-alist `(height . ,(max-frame-rows))))

;; (if ns-initialized
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (when (eq (framep frame) 'ns)
;;                   (ns-do-applescript "tell application \"Emacs\" to activate")
;;                   (set-frame-position frame 0 0)))))

(global-set-key (kbd "C-+")
                (lambda ()
                  (interactive)
                  (when (display-graphic-p)
                    (set-frame-height (selected-frame) (max-frame-rows)))))
