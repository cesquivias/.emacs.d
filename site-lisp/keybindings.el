(global-set-key (kbd "C-M-<backspace>") (lambda ()
                                          (interactive)
                                          (back-to-indentation)
                                          (kill-line)))
(global-set-key (kbd "C-z") 'iy-go-to-char) ;; who needs suspend-frame?

(global-set-key (kbd "M-C-;") 'uncomment-region)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c p") 'package-list-packages)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c l") 'insert-lambda)

;; Windmove
(global-set-key (kbd "M-P") 'windmove-up)
(global-set-key (kbd "M-N") 'windmove-down)
(global-set-key (kbd "M-B") 'windmove-left)
(global-set-key (kbd "M-F") 'windmove-right)
