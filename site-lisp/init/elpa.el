(setq packages-loaded nil)

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
