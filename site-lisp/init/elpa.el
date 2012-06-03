(setq packages-loaded nil)

(defun require-or-install (library-name &optional min-version)
  (interactive "SLibrary: ")
  (when (not packages-loaded)
    (message "Loading ELPA information..")
    (package-refresh-contents)
    (setq packages-loaded t))
  (unless (package-installed-p library-name min-version)
    (message (format "%s was not found. Installing %s from ELPA"
                     library-name library-name))
    (condition-case ex
        (package-install library-name)
      (error (message (error-message-string ex))))))
