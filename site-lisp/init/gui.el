(if (functionp 'x-initialize-window-system)
  (x-initialize-window-system) ;; In case we're starting as daemon
  (setq x-initialized nil)) ;; No x functions, x never initialized

(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode 'right)

;; Adjust GUI window
(let* ((x? (eq 'x (window-system)))
       (top (if x? 23 0)) ;; x doesn't compensate for the frame's title bar
       (left (if x? 4 0))) ;; x doesn't compensate for the frame's chrome
  (add-to-list 'default-frame-alist `(top . ,top))
  (add-to-list 'initial-frame-alist `(left . ,left)))
(add-to-list 'default-frame-alist '(width . 80))

(defun max-frame-rows (&optional frame)
  (/ (- (x-display-pixel-height) 70)
     (/ (frame-pixel-height frame) (frame-height frame))))

(when (or (window-system) x-initialized)
  (let ((rows (max-frame-rows))
        (x-rows-offset 4))
    (add-to-list 'initial-frame-alist
                 (cons 'height (if (eq 'x (window-system))
                                 (- rows x-rows-offset)
                                 rows))))
  (add-hook 'after-make-frame-functions
              (lambda (frame)
                (let* ((f (framep frame))
                       (rows (max-frame-rows frame)))
                  (when (or (eq f 'x) (eq f 'w32) (eq f 'ns))
                    (set-frame-height frame rows)))))
  (global-set-key (kbd "C-+")
                  (lambda ()
                    (interactive)
                    (set-frame-height (selected-frame) (max-frame-rows)))))

(when (or (eq (window-system) 'x) x-initialized)
  (add-to-list 'default-frame-alist
               '(font . "-unknown-Liberation Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")))
