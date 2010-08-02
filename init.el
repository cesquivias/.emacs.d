;; ELPA
(when (load (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(setq load-path (cons "~/.emacs.d/site-lisp" load-path))
(setq load-path (cons "~/.emacs.d/local-lisp" load-path))

;; built-in
(require 'uniquify)
(require 'rst)

;; platform dependent
(require 'paredit)
(require 'yaml-mode)
(require 'color-theme)
(color-theme-initialize)
(color-theme-ld-dark)

;; site-lisp
(require 'blank-mode)
(require 'django-html-mode)

(iswitchb-mode t)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(global-font-lock-mode t)

(setq frame-title-format `("%b@" ,(system-name))) ;; buffer-name@hostname
(setq transient-mark-mode t) ;; default in emacs23
(setq column-number-mode t) ;; show colums next to line numbers
(setq read-file-name-completion-ignore-case t) ;; filenames are case insensitive
(setq read-buffer-completion-ignore-case t) ;; buffer names are case insensitive
(setq confirm-kill-emacs 'yes-or-no-p) ;; confirm kill-emacs
(setq inhibit-splash-screen t) ;; useless
(setq uniquify-buffer-name-style 'reverse) ;; filename/parent
(setq line-move-visual nil) ;; Move logical--not visual, word-wrapped--lines
(setq even-window-heights nil) ;; stop commands like C-x 4 b from resizing
(setq eval-expression-print-length nil) ;; dont' truncate eval prints
(setq indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq tab-width 4)
(setq tab-always-indent t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-always-indent t)
(setq-default js2-mirror-mode nil)
(setq-default even-window-heights nil)

(fset 'yes-or-no-p 'y-or-n-p) ;; Use shorter y/n prompt
(put 'dired-find-alternate-file 'disabled nil) ;; Enable 'a' shortcut

;; Clipboard compatbility
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)


(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(add-hook 'text-mode-hook
          (lambda ()
            (setq tab-width 4)))

(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq require-final-newline t)))

(add-hook 'java-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq tab-always-indent t)))

(add-hook 'js2-mode-hook
          (lambda ()
            (setq tab-width 4)))

(add-to-list 'emacs-lisp-mode-hook (lambda ()
                                     (paredit-mode +1)))

;;; RST fixing
(set-face-background 'rst-level-1-face "#000")
(set-face-background 'rst-level-2-face "#000")
(set-face-background 'rst-level-3-face "#000")
(set-face-background 'rst-level-4-face "#000")
(set-face-background 'rst-level-5-face "#000")
(set-face-background 'rst-level-6-face "#000")

(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode 'right)


(when (eq (window-system) 'x)
  (add-to-list 'default-frame-alist '(font . "-unknown-Liberation Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")))
