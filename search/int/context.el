(defun read-file-content (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-string)))

(defvar int-context
  (list
   :read-file-content #'read-file-content))
