
;;;; Site-Lisp: checked-in directory with libraries not available on ELPA
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
(require 'dos)
(require 'django-html-mode)

(load "init/elpa")
(require 'package)
(setq package-archives '(
                         ;; ("ELPA" . "http://tromey.com/elpa/") 
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;;;; Local Lisp: Libraries not checked into version control
(add-to-list 'load-path (expand-file-name "~/.emacs.d/local-lisp"))
(let ((local-init "~/.emacs.d/local-lisp/init.el"))
  (if (file-exists-p local-init)
      (load local-init)))

;;;; Built-In
(require 'uniquify)
(require 'rst)
(require 'whitespace)

;;;; Platform Dependent
(require-or-install 'paredit)
(require-or-install 'yaml-mode)
(require-or-install 'js2-mode) ;; js2-mode is screwing up
(require-or-install 'clojure-mode)
(require-or-install 'org)
(require-or-install 'cider)
(require-or-install 'htmlize)
(require-or-install 'deft)
(require-or-install 'android-mode)
(require-or-install 'geiser)
(require-or-install 'iy-go-to-char)
(require-or-install 'magit)
(require-or-install 'markdown-mode)
(require-or-install 'haskell-mode)
(require-or-install 'groovy-mode)
(require-or-install 'fish-mode)
(require-or-install 'ox-reveal)
(require-or-install 'ace-window)
;; (require-or-install 'yasnippet-bundle)
;; (require-or-install 'yas-jit)
(if (>= emacs-major-version 24)
    (load-theme 'wombat t)
  (require-or-install 'color-theme)
  (color-theme-ld-dark))

;;; Start Server When not in daemon mode
(unless (daemonp)
  (server-start))

(iswitchb-mode t)
(show-paren-mode t)
(which-function-mode t)
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(global-font-lock-mode t)

;; Saving Emacs state between startups
(setq desktop-dirname (expand-file-name "~/.emacs.d/desktop/")
      desktop-path (list desktop-dirname)
      desktop-base-file-name "emacs-desktop")
(make-directory desktop-dirname t)
(desktop-save-mode t)

;; Recent files
(require 'recentf)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 15)
(recentf-mode)

(setq default-directory "~/") ;; Always start in home directory
(setq custom-file "~/.emacs.d/site-lisp/custom.el")
(setq frame-title-format `("%b@" ,(system-name))) ;; buffer-name@hostname
(setq blink-cursor-mode t) ;; Don't know why this isn't on on Macs
(setq backup-directory-alist `(("." . "~/.emacs.d/backups"))) ;; not in dirs
(setq backup-by-copying t)
(setq transient-mark-mode t) ;; default in emacs23
(setq column-number-mode t) ;; show columns next to line numbers
(setq read-file-name-completion-ignore-case t) ;; filenames are case insensitive
(setq read-buffer-completion-ignore-case t) ;; buffer names are case insensitive
(setq iswitchb-case t) ;; iswitchb ignores case
(setq confirm-kill-emacs 'yes-or-no-p) ;; to prevent those accidental exits
(setq inhibit-splash-screen t) ;; useless
(setq uniquify-buffer-name-style 'reverse) ;; filename/parent
;; (setq line-move-visual nil) ;; Move logical--not visual, word-wrapped--lines
(setq scroll-preserve-screen-position t) ;; scrolling back and forth keeps place
(setq even-window-heights nil) ;; stop commands like C-x 4 b from resizing
(setq eval-expression-print-length nil) ;; don't truncate eval prints
(setq text-scale-mode-step 1.05) ;; Increase font size 5% each time
(setq delete-selection-mode t) ;; Character inserts delete region
(setq show-paren-style 'parenthesis) ;; Only highlight the parentheses
(setq c-basic-offset 4) ;; Indent 4 spaces for C-style modes
(setq tab-always-indent t) ;; TAB key always indents
(setq tab-stop-list (number-sequence 4 80 4)) ;; Setting tab stop at 4 spaces
(setq kill-whole-line t) ;; Kill whole line when at the beginning of the line
(setq dired-listing-switches "-alh")
(setq compilation-scroll-output t) ;; Auto-scroll buffer with new output
(setq-default dabbrev-case-replace 'nil) ;; Don't change the case when expanding
(setq-default dabbrev-case-fold-search 'nil) ;; Don't use different-cased words
(setq-default major-mode 'org-mode) ;; Default major mode is org-mode
(setq-default indent-tabs-mode nil) ;; Do not indent tabs
(setq-default tab-width 4) ;; Default tab is generally 4 spaces
(setq-default js2-mirror-mode nil) ;; Don't auto-insert the closing paren, brace
(setq-default require-final-newline t) ;; All files should end in newline
(setq-default cursor-in-non-selected-windows nil) ;; blinking is annoying in bg
(setq-default sentence-end-double-space nil) ;; 1 space separates senteces

(fset 'yes-or-no-p 'y-or-n-p) ;; Use shorter y/n prompt
(put 'dired-find-alternate-file 'disabled nil) ;; Enable 'a' shortcut in dired
(put 'set-goal-column 'disabled nil) ;; Enable C-x C-n
(if (fboundp 'global-subword-mode)
    (global-subword-mode t)) ;; camelCase names are split into words

;;;; Clipboard Compatibility
(when (eq (window-system) 'x)
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;;;; Put tooltips in minibuffer
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.rkt$" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.just$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.jie$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(add-hook 'text-mode-hook
          (lambda ()
            (setq tab-width 4)))

(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq require-final-newline t)))

(add-hook 'java-mode-hook
          (lambda ()
            ;; Make annotations comments
            (setq c-comment-start-regexp "(@|/(/|[*][*]?))")
            (modify-syntax-entry ?@ "< b" java-mode-syntax-table)

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
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode +1)))
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(add-hook 'scheme-mode-hook
          (lambda ()
            (paredit-mode +1)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(eval-after-load "eshell"
  '(eshell-remove-from-window-buffer-names))(eval-after-load "eshell"
  '(eshell-remove-from-window-buffer-names))

;;; XML pretty printing

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
               (local-set-key (kbd "M-h") 'nxml-mark-token-after)
               (local-set-key (kbd "M-{") 'nxml-backward-element)
               (local-set-key (kbd "M-}") 'nxml-forward-element)
               (local-set-key (kbd "C-M-q") 'bf-pretty-print-xml-region)
               ;; sgml-mode made this shortcut second hand
               (local-set-key (kbd "C-c /") 'nxml-finish-element)))
(setq-default nxml-child-indent 4)

;;;; Custom variables
(setq lib-directory (file-name-as-directory
                     (expand-file-name "~/.emacs.d/lib")))
(make-directory lib-directory t)

(setq temp-directory (file-name-as-directory
                      (expand-file-name "~/.emacs.d/tmp")))
(if (file-exists-p temp-directory)
    (delete-directory temp-directory t))
(make-directory temp-directory)

;;;; Custom functions
(load "utils")

;;;; New Keyboard Shortcuts
(load "keybindings")

;;; RST fixing
(if (or (> emacs-major-version 24)
        (and (eq emacs-major-version 24) (>= emacs-minor-version 3)))
    (progn
     (set-face-background 'rst-level-1 "#000")
     (set-face-background 'rst-level-2 "#000")
     (set-face-background 'rst-level-3 "#000")
     (set-face-background 'rst-level-4 "#000")
     (set-face-background 'rst-level-5 "#000")
     (set-face-background 'rst-level-6 "#000"))
  (set-face-background 'rst-level-1-face "#000")
  (set-face-background 'rst-level-2-face "#000")
  (set-face-background 'rst-level-3-face "#000")
  (set-face-background 'rst-level-4-face "#000")
  (set-face-background 'rst-level-5-face "#000")
  (set-face-background 'rst-level-6-face "#000"))

;;;; Eshell
(setq eshell-directory-name "~/.emacs.d/eshell/")

;;;; Deft
(setq deft-extension "org")
(setq deft-directory "~/org/deft/")
(make-directory deft-directory 'parents) ;; make sure it exists
(setq deft-text-mode 'org-mode)
(global-set-key (kbd "C-c d") 'deft)

;;;; org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode t)
            (local-set-key (kbd "C-<") 'org-metaleft)
            (local-set-key (kbd "C->") 'org-metaright)))
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(setq org-startup-folded 'showall
      org-goto-auto-isearch nil
      org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (js . t)
   (sh . t)
   (plantuml . t)
   (ditaa . t)
   (dot . t)))

;;; PlantUML plugin
(let ((plantuml-jar (expand-file-name "~/.emacs.d/lib/plantuml.jar"))
      (plantuml-url "http://sourceforge.net/projects/plantuml/files/plantuml.jar/download"))
  (unless (file-exists-p plantuml-jar)
    (url-copy-file plantuml-url plantuml-jar))
  (setq org-plantuml-jar-path plantuml-jar))

;;; ditaa plugin
(let ((ditaa-jar (concat lib-directory "ditaa.jar")))
  (unless (file-exists-p ditaa-jar)
    (let ((ditaa-url "http://sourceforge.net/projects/ditaa/files/ditaa/0.9/ditaa0_9.zip/download")
          (tmp-f (concat temp-directory "ditaa.zip"))
          (tmp-d (file-name-as-directory (concat temp-directory "ditaa"))))
      (url-copy-file ditaa-url tmp-f t) ;; download zip 
      (and
       (unzip tmp-f tmp-d)
       (rename-file (concat tmp-d (file-name-completion "ditaa" tmp-d))
                    ditaa-jar)    ;; move jar
       (delete-directory tmp-d t) ;; delete tmp dir
       (delete-file tmp-f)) ;; delete zip
      ))
  (setq org-ditaa-jar-path ditaa-jar))

;;; Scheme
(setq scheme-program-name "racket")

;;;; GUI
(load "init/gui")

;;;; Set eclipse indentation as default for java
(load "eclipse-indent")
(add-hook 'kill-emacs-query-functions
          'custom-prompt-customize-unsaved-options)
(load custom-file)
