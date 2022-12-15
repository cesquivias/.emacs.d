;; (global-set-key (kbd "C-M-<backspace>") (lambda ()
;;                                           (interactive)
;;                                           (back-to-indentation)
;;                                           (kill-line)))

(global-set-key (kbd "M-C-;") 'uncomment-region)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c p") 'package-list-packages)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c l") 'insert-lambda)
(global-set-key (kbd "C-c C-f") 'find-file-at-point)

;; Windmove
(global-set-key (kbd "M-P") 'windmove-up)
(global-set-key (kbd "M-N") 'windmove-down)
(global-set-key (kbd "M-B") 'windmove-left)
(global-set-key (kbd "M-F") 'windmove-right)

(global-set-key (kbd "C-x o") 'ace-window)

(global-set-key (kbd "C-c 2") 'set-frame-double-width)
(global-set-key (kbd "C-c 3") 'double-frame-right)
