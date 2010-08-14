(let ((package-el  "~/.emacs.d/elpa/package.el"))
  (if (file-exists-p package-el)
      (when (load (expand-file-name package-el))
	(package-initialize))
      (let ((buffer (url-retrieve-synchronously
		     "http://tromey.com/elpa/package-install.el"))
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

(defun require-or-install (library-name &optional package-name)
  (unless (require library-name nil t)
    (let ((package (if package-name package-name library-name)))
      (message (format "%s was not found. Installing %s from ELPA"
                       library-name package))
      (package-install package))))
