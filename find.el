(setq lexical-binding t)

(defun find/ns-name ()
  (let ((orig-point (point)))
    (goto-char (point-min))
    (forward-thing 'symbol 2)
    (let ((symbol (thing-at-point 'symbol)))
      (goto-char orig-point)
      symbol)))

(defun find/insert-results (current remaining)
  (when current
    (insert current)
    (insert "\n")
    (find/insert-results (car remaining) (cdr remaining))))

(defun find/escape-special-characters (str &optional result idx)
  "Escape all characters with backslash that are not word characters.
Required by `ag`
STR the string to escape.
RESULT The result accumulator for recursion.
IDX the next character in STR"
  (let ((idx (or idx 0))
        (result (or result "")))
    (if (> (length str) idx)
        (let ((ascii (aref str idx)))
          (if (replace/special-character? ascii)
              (replace/escape-special-characters str (concat result (string 92 ascii)) (+ idx 1))
            (replace/escape-special-characters str (concat result (string ascii)) (+ idx 1))))
      result)))

(defun find/get-filenames (ag-hit-list result)
  "Recursively parse lines from AG-HIT-LIST to filenames into RESULT ."
  (if (not ag-hit-list)
      result
    (let* ((next-hit (car ag-hit-list))
	   (file-name (car (split-string next-hit ":")))
	   (new-result (cons file-name result))
	   (new-result (delete-dups new-result)))
      (find/get-filenames (cdr ag-hit-list) new-result))))

(defun find/get-files (str)
  (let* ((ns-name (find/ns-name))
         (command (format "ag %s %s" (format "\"%s\"" (find/escape-special-characters str)) (projectile-project-root)))
         (result-string (shell-command-to-string str))
         (result-string (string-trim result-string)))
    (file-list (find/ag-result-string->list result-string))))

(defun find/ag-result-string->list (result-string)
  "Turn a ag RESULT-STRING into a list of filenames."
  (let ((lines (split-string result-string "\n")))
    (find/get-filenames lines '())))

(defun find-reference ()
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
         (file-list (find/get-files symbol))
         (buffer (get-buffer-create "*References*")))
    (switch-to-buffer buffer)
    (find/insert-results (car file-list) (cdr file-list))))

(provide 'find)
;;; find.el ends here
