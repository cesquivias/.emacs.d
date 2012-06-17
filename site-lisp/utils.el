(defun shell-command-if-exists (exec args)
  (if (executable-find exec)
      (shell-command (concat exec " " args))
    (message (concat "'" exec "' not found. Please install."))))

(defun wget (url)
  "Download file for the given URL, using the same filename."
  (url-copy-file url (url-file-nondirectory url)))

(defun unzip (zip-file odir)
  "Unzip a .zip file into `odir' directory specified. Try to use whatever tool is available on the system to unzip a file. "
  (make-directory odir t)
  (if (memq system-type '(ms-dos windows-nt))
      (let ((tmp-zip (concat (file-name-as-directory odir)
                             (file-name-nondirectory zip-file))))
        (copy-file zip-file tmp-zip t)
        (let ((d default-directory))
          (cd odir)
          (shell-command (concat "jar xf " tmp-zip))
          (cd d))
        (delete-file tmp-zip))
    (shell-command-if-exists "unzip" (concat "-d " odir " " zip-file))))
