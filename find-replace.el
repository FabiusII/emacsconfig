;;; package --- Summary
;;; Commentary:
;;; Text based search and replace

;;; Code:
(setq lexical-binding t)

(defun rename-local-symbol ()
  "Renaming a symbol in the current function based on its text content alone.
Before renaming each single occurance, the user is asked for permission.
Only the user can detect if the text is actually a reference to the symbol."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (let* ((prompt (format "replace %s with: " symbol))
	     (replacement (read-string prompt))
	     (scope (bounds-of-thing-at-point 'defun)))
	(query-replace (regexp-quote symbol) replacement 'delimited (car scope) (cdr scope))))))

(defun rename-in-buffer ()
  "Renaming a symbol in the current buffer based on its text content alone.
Before renaming each single occurance, the user is asked for permission.
Only the user can detect if the text is actually a reference to the symbol."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (let* ((old-point (point))
             (prompt (format "replace %s with: " symbol))
	     (replacement (read-string prompt)))
        (goto-char (point-min))
        (query-replace symbol replacement)
        (goto-char old-point)))))

(defun find-replace/get-filenames (ag-hit-list result)
  "Recursively parse lines from AG-HIT-LIST to filenames into RESULT ."
  (if (not ag-hit-list)
      result
    (let* ((next-hit (car ag-hit-list))
	   (file-name (car (split-string next-hit ":")))
	   (new-result (cons file-name result))
	   (new-result (delete-dups new-result)))
      (find-replace/get-filenames (cdr ag-hit-list) new-result))))

(defun find-replace/ag-result-string->list (result-string)
  "Turn a ag RESULT-STRING into a list of filenames."
  (let ((lines (split-string result-string "\n")))
    (find-replace/get-filenames lines '())))

(defun find-replace/open-and-replace-in-files (files symbol replacement)
  "Opens each file in FILES and asks to replace SYMBOL with REPLACEMENT."
  (when files
    (find-file (car files))
    (goto-char (point-min))
    (query-replace symbol replacement)
    (save-buffer)
    (find-replace/open-and-replace-in-files (cdr files) symbol replacement)))

(defun find-replace/special-character? (char)
  "True if CHAR is not a word character."
  (or (< char 65)
      (and (> char 90) (< char 97))
      (> char 122)))

(defun find-replace/escape-special-characters (str &optional result idx)
  "Escape all characters with backslash that are not word characters.
Required by `ag`
STR the string to escape.
RESULT The result accumulator for recursion.
IDX the next character in STR"
  (let ((idx (or idx 0))
        (result (or result "")))
    (if (> (length str) idx)
        (let ((ascii (aref str idx)))
          (if (find-replace/special-character? ascii)
              (find-replace/escape-special-characters str (concat result (string 92 ascii)) (+ idx 1))
            (find-replace/escape-special-characters str (concat result (string ascii)) (+ idx 1))))
      result)))

(defun rename-in-project ()
  "Renaming a symbol in the current buffer based on its text content alone.
Before renaming each single occurance, the user is asked for permission.
Only the user can detect if the text is actually a reference to the symbol.
Requies and projectile and ag (aka the_silver_searcher).
The latter must be installed on your system, not necessarily EMACS."
  (interactive)
  (when (and (featurep 'projectile)
	     (featurep 'subr-x))
    (require 'projectile)
    (require 'subr-x)
    (let ((old-point (point))
          (old-buffer (current-buffer))
          (symbol (thing-at-point 'symbol)))
      (when symbol
	(let* ((prompt (format "replace %s with: " symbol))
	       (replacement (read-string prompt))
	       (command (format "ag %s %s" (format "\"%s\"" (find-replace/escape-special-characters symbol)) (projectile-project-root)))
	       (result-string (shell-command-to-string command))
	       (result-string (string-trim result-string))
	       (file-list (find-replace/ag-result-string->list result-string)))
	  (find-replace/open-and-replace-in-files file-list symbol replacement)
	  (when old-buffer
	    (switch-to-buffer old-buffer)
            (goto-char old-point)))))))

(provide 'find-replace)
;;; find-replace.el ends here
