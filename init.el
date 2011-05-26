;;;; Built-In
(require 'uniquify)
(require 'rst)

;;;; Site-Lisp: checked-in directory with libraries not available on ELPA
(setq load-path (cons "~/.emacs.d/site-lisp" load-path))
(require 'dos)
(require 'django-html-mode)
(require 'android-mode)
(load "init/elpa.el")

;;;; Platform Dependent
(setq load-path (cons "~/.emacs.d/local-lisp" load-path))
(require-or-install 'blank-mode)
(require-or-install 'paredit)
(require-or-install 'yaml-mode)
;; (require-or-install 'js2-mode) ;; js2-mode is screwing up
(require-or-install 'swank-clojure) ;; Installs clojure, slime, slime-repl
(require-or-install 'yasnippet-bundle)
(if (require 'color-theme nil t)
    (progn
      (if (fboundp 'color-theme-initialize) ;; Doesn't exist in .deb package
          (color-theme-initialize))
      (color-theme-ld-dark))
  (message "color-theme not installed"))

(iswitchb-mode t)
(show-paren-mode t)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(global-font-lock-mode t)

(setq default-directory "~/") ;; Always start in home directory
(setq frame-title-format `("%b@" ,(system-name))) ;; buffer-name@hostname
;; Don't litter directories
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq transient-mark-mode t) ;; default in emacs23
(setq column-number-mode t) ;; show columns next to line numbers
(setq read-file-name-completion-ignore-case t) ;; filenames are case insensitive
(setq read-buffer-completion-ignore-case t) ;; buffer names are case insensitive
(setq confirm-kill-emacs 'yes-or-no-p) ;; to prevent those accidental exits
(setq inhibit-splash-screen t) ;; useless
(setq uniquify-buffer-name-style 'reverse) ;; filename/parent
(setq line-move-visual nil) ;; Move logical--not visual, word-wrapped--lines
(setq even-window-heights nil) ;; stop commands like C-x 4 b from resizing
(setq eval-expression-print-length nil) ;; don't truncate eval prints
(setq text-scale-mode-step 1.05) ;; Increase font size 5% each time
(setq delete-selection-mode t) ;; Character inserts delete region
(setq show-paren-style 'parenthesis) ;; Only highlight the parentheses
(setq c-basic-offset 4) ;; Indent 4 spaces for C-style modes
(setq tab-always-indent t) ;; TAB key always indents
(setq tab-stop-list (number-sequence 4 80 4)) ;; Setting tab stop at 4 spaces
(setq kill-whole-line t) ;; Kill whole line when at the beginning of the line
(setq-default major-mode 'org-mode) ;; Default major mode is org-mode
(setq-default indent-tabs-mode nil) ;; Do not indent tabs
(setq-default tab-width 4) ;; Default tab is generally 4 spaces
(setq-default js2-mirror-mode nil) ;; Don't auto-insert the closing paren, brace
(setq-default require-final-newline t) ;; All files should end in newline

(fset 'yes-or-no-p 'y-or-n-p) ;; Use shorter y/n prompt
(put 'dired-find-alternate-file 'disabled nil) ;; Enable 'a' shortcut in dired
(put 'set-goal-column 'disabled nil) ;; Enable C-x C-n
(global-subword-mode t) ;; camelCase names are split into words

;;;; Clipboard Compatibility
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
            (setq tab-width 4)
            (local-set-key (kbd "M-;") 'comment-region)))

(add-hook 'clojure-mode-hook
          (lambda ()
            (paredit-mode +1)
            (local-set-key (kbd "C-x E") 'slime-eval-buffer)))

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                     (paredit-mode +1)))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-<") 'org-metaleft)
            (local-set-key (kbd "C->") 'org-metaright)))

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
   http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
   this.  The function inserts linebreaks to separate tags that have
   nothing but whitespace between them.  It then indents the markup
   by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
      (backward-char) (insert "\n"))
    (indent-region begin end)))

(add-hook 'nxml-mode-hook
             (lambda ()
               (local-set-key (kbd "C-M-q") 'bf-pretty-print-xml-region)
               ;; sgml-mode made this shortcut second hand
               (local-set-key (kbd "C-c /") 'nxml-finish-element)))

;;;; New Keyboard Shortcuts
(global-set-key (kbd "M-C-;") 'uncomment-region)

;;; RST fixing
(set-face-background 'rst-level-1-face "#000")
(set-face-background 'rst-level-2-face "#000")
(set-face-background 'rst-level-3-face "#000")
(set-face-background 'rst-level-4-face "#000")
(set-face-background 'rst-level-5-face "#000")
(set-face-background 'rst-level-6-face "#000")

;;;; Eshell
(setq eshell-directory-name "~/.emacs.d/eshell/")

;;;; Local Lisp: Libraries not checked into version control
(let ((local-init "~/.emacs.d/local-lisp/init.el"))
  (if (file-exists-p local-init)
      (load local-init)))

;;;; GUI
(load "init/gui.el")

;;;; Utils
(load "utils.el")
