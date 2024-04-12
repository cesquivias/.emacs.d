(let ((has-pyright (executable-find "pyright")))
  (use-package lsp-pyright
    :if has-pyright
    :hook (python-mode . (lambda ()
                           (require 'lsp-pyright)
                           (lsp-deferred))))
  (unless has-pyright
    (cl-flet ((display-message ()
                (message "Cannot start lsp-mode. Pyright not installed")))
      (message "Pyright not installed")
      (add-hook 'python-mode-hook #'display-message)
      (if (not (version< emacs-version "29.1"))
          (add-hook 'python-ts-mode-hook #'display-message)))))


