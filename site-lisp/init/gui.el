(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode 'right)

;; Adjust GUI window
(add-to-list 'default-frame-alist '(top . 23))
(add-to-list 'default-frame-alist '(width . 80))

(add-to-list 'initial-frame-alist '(left . 4))
(defun max-frame-rows (&optional frame)
  (/ (- (x-display-pixel-height) 70)
     (/ (frame-pixel-height frame) (frame-height frame))))

(if (window-system)
  (add-to-list 'initial-frame-alist (cons 'height (max-frame-rows)))
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (set-frame-height frame (max-frame-rows frame)))))

(when (eq (window-system) 'x)
  (add-to-list 'default-frame-alist
               '(font . "-unknown-Liberation Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")))
