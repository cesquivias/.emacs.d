;;;; Utility functions

(defun wget (url)
  "Download file for the given URL, using the same filename."
  (url-copy-file url (url-file-nondirectory url)))

(defun unzip (zip-file odir)
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
    (shell-command (concat "unzip -d " odir " " zip-file))))
