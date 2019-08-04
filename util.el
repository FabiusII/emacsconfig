;;; package --- Summary
;;; Commentary:
;;; Collection of utility functions

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
	(query-replace symbol replacement 'delimited (car scope) (cdr scope))))))

(defun rename-in-buffer ()
  "Renaming a symbol in the current buffer based on its text content alone.
Before renaming each single occurance, the user is asked for permission.
Only the user can detect if the text is actually a reference to the symbol."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (let* ((prompt (format "replace %s with: " symbol))
	     (replacement (read-string prompt)))
	(query-replace symbol replacement 'delimited (point-min) (point-max))))))

(defun util/get-filenames (ag-hit-list result)
  "Recursively parse lines from AG-HIT-LIST to filenames into RESULT ."
  (if (not ag-hit-list)
      result
    (let* ((next-hit (car ag-hit-list))
	   (file-name (car (split-string next-hit ":")))
	   (new-result (cons file-name result))
	   (new-result (delete-dups new-result)))
      (util/get-filenames (cdr ag-hit-list) new-result))))

(defun util/ag-result-string->list (result-string)
  "Turn a ag RESULT-STRING into a list of filenames."
  (let ((lines (split-string result-string "\n")))
    (util/get-filenames lines '())))

(defun util/open-and-replace-in-files (files symbol replacement)
  "Opens each file in FILES and asks to replace SYMBOL with REPLACEMENT."
  (when files
    (find-file (car files))
    (query-replace symbol replacement 'delimited (point-min) (point-max))
    (save-buffer)
    (util/open-and-replace-in-files (cdr files) symbol replacement)))

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
    (let ((old-buffer (current-buffer))
	  (symbol (thing-at-point 'symbol)))
      (when symbol
	(let* ((prompt (format "replace %s with: " symbol))
	       (replacement (read-string prompt))
	       (command (format "ag %s %s" symbol (projectile-project-root)))
	       (result-string (shell-command-to-string command))
	       (result-string (string-trim result-string))
	       (file-list (util/ag-result-string->list result-string)))
	  (util/open-and-replace-in-files file-list symbol replacement)
	  (when old-buffer
	    (switch-to-buffer old-buffer)))))))

(provide 'util)
;;; util.el ends here
