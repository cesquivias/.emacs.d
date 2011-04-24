;;;; Utility functions

(defun wget (url)
  "Download file for the given URL, using the same filename."
  (url-copy-file url (url-file-nondirectory url)))
