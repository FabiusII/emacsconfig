;;; package --- Summary
;;; Commentary:
;;; Text based search

;;; Code:
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

(defun find/special-character? (char)
  "True if CHAR is not a word character."
  (or (< char 65)
      (and (> char 90) (< char 97))
      (> char 122)))

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
          (if (find/special-character? ascii)
              (find/escape-special-characters str (concat result (string 92 ascii)) (+ idx 1))
            (find/escape-special-characters str (concat result (string ascii)) (+ idx 1))))
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

(defun find/namespace-imported? (ns)
  (message "%s" (string-match (format "(ns .*\\(\n.*\\)*.*:require .*\\(\n.*\\)*.* :refer \\[.*%s.*\\]" ns) (buffer-string))))

(defun namespace-imported? ()
  (interactive)
  (find/namespace-imported? (read-string "name")))

(defun find/ag-result-string->list (result-string)
  "Turn a ag RESULT-STRING into a list of filenames."
  (let* ((lines (split-string result-string "\n"))
         (files (find/get-filenames lines '())))
    (seq-filter )))

(defun find-files (str)
  (let* ((ns-name (find/ns-name))
         (command (format "ag %s %s" (format "\"%s\"" (find/escape-special-characters str)) (projectile-project-root)))
         (result-string (shell-command-to-string command))
         (result-string (string-trim result-string)))
    (find/ag-result-string->list result-string)))

(defun read-file ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents "/Users/fabianhanselmann/repos/emacsconfig/find.el")
    (let ((ns-form (thing-at-point 'defun)))
      (message (format "test: %s" ns-form)))
    (buffer-string)))

(defun find-reference ()
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
         (file-list (find-files symbol))
         (buffer (get-buffer-create "*References*")))
    (switch-to-buffer buffer)
    (find/insert-results (car file-list) (cdr file-list))))

(provide 'find)
;;; find.el ends here
