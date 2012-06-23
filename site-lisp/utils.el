(defun wget (url)
  "Download file for the given URL, using the same filename."
  (url-copy-file url (url-file-nondirectory url)))

(defun unzip-with-jar (zip-file odir)
  "Unzip file using jar command"
  (make-directory odir t)
  (let* ((original-dir default-directory)
         (zfilename (file-name-nondirectory zip-file))
         (original-zip-dir (or (file-name-directory zip-file)
                               original-dir)))
    (rename-file zip-file odir)
    (cd odir)
    (shell-command (concat "jar xf " zfilename))
    (rename-file zfilename original-zip-dir)
    (cd original-dir)))

(defun unzip (zip-file odir)
  "Try to use whatever tool is available on the system to unzip a file. "
  (if (memq system-type '(ms-dos windows-nt))
      (unzip-with-jar zip-file odir)
      (shell-command (concat "unzip -d " odir " " zip-file))))
