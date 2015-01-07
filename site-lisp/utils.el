(defun shell-command-if-exists (exec args)
  "Run `exec' if the file exists. If not, output an error message."
  (if (executable-find exec)
      (shell-command (concat exec " " args))
    (message (concat "'" exec "' not found. Please install."))))

(defun wget (url)
  "Download file for the given URL, using the same filename."
  (interactive "MURL: ")
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
  "Unzip a .zip file into `odir' directory specified. Try to use whatever tool is available on the system to unzip a file. "
  (make-directory odir t)
  (if (memq system-type '(ms-dos windows-nt))
      (unzip-with-jar zip-file odir)
    (shell-command-if-exists "unzip" (concat "-d " odir " " zip-file))))

(defun insert-lambda ()
  "Insert a λ character at point."
  (interactive)
  (insert-char ?\λ 1))
