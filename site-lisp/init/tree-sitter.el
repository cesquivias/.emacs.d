;; Check if we can build tree-sitter parsing libraries
(when (and (not (version< emacs-version "29.1"))
           (executable-find "cc")
           (not (string-empty-p
                 (shell-command-to-string
                  "ldconfig -p | grep libtree-sitter"))))
  (use-package treesit
    :preface
    (defun mp-setup-install-grammars ()
      "Install Tree-sitter grammars if they are absent."
      (interactive)
      (dolist (grammar
               '(
                 (css "https://github.com/tree-sitter/tree-sitter-css")
                 (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
                 (python "https://github.com/tree-sitter/tree-sitter-python")
                                        ;(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
                 (yaml "https://github.com/ikatyang/tree-sitter-yaml")
                 ))
        (add-to-list 'treesit-language-source-alist grammar)
        ;; Only install `grammar' if we don't already have it
        ;; installed. However, if you want to *update* a grammar then
        ;; this obviously prevents that from happening.
        (unless (treesit-language-available-p (car grammar))
          (treesit-install-language-grammar (car grammar)))))

    ;; Optional, but recommended. Tree-sitter enabled major modes are
    ;; distinct from their ordinary counterparts.
    ;;
    ;; You can remap major modes with `major-mode-remap-alist'. Note
    ;; that this does *not* extend to hooks! Make sure you migrate them
    ;; also
    (dolist (mapping '((python-mode . python-ts-mode)
                       (css-mode . css-ts-mode)
                       (typescript-mode . tsx-ts-mode)
                       (json-mode . json-ts-mode)
                       (js-mode . js-ts-mode)
                       (css-mode . css-ts-mode)
                       (yaml-mode . yaml-ts-mode)))
      (add-to-list 'major-mode-remap-alist mapping))

    :config
    (mp-setup-install-grammars))

  (use-package combobulate
    :preface
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (setq combobulate-key-prefix "C-c o")

    :config
    (unbind-key (kbd "M-N") combobulate-key-map)
    (unbind-key (kbd "M-P") combobulate-key-map)

    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    :hook ((python-ts-mode . combobulate-mode)
           (js-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("~/.emacs.d/local-lisp/combobulate"))
  )
