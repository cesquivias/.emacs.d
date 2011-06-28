(setq packages-loaded nil)

(let ((package-el  "~/.emacs.d/elpa/package.el"))
  (if (file-exists-p package-el)
      (when (load (expand-file-name package-el))
        (package-initialize))
    (let ((buffer (url-retrieve-synchronously
		   "http://repo.or.cz/w/emacs.git/blob_plain/1a0a666f941c99882093d7bd08ced15033bc3f0c:/lisp/emacs-lisp/package.el")) 
          (ifile user-init-file))
      (setq user-init-file nil)
      (unwind-protect
          (save-excursion
            (set-buffer buffer)
            (goto-char (point-min))
            (re-search-forward "^$" nil 'move)
            (eval-region (point) (point-max))
            (kill-buffer (current-buffer)))
        (setq user-init-file ifile)))))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defun require-or-install (library-name &optional package-name)
  (unless (require library-name nil t)
    (let ((package (if package-name package-name library-name)))
      (when (not packages-loaded)
        (message "Loading ELPA information..")
        (package-refresh-contents)
        (setq packages-loaded t))
      (message (format "%s was not found. Installing %s from ELPA"
                       library-name package))
      (package-install package))))
