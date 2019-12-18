;;; package --- Summary
;;; Commentary:
;;; Text based search and replace

;;; Code:
(setq lexical-binding t)

(load-file "./find.el")

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

(defun replace/open-and-replace-in-files (files symbol replacement)
  "Opens each file in FILES and asks to replace SYMBOL with REPLACEMENT."
  (when files
    (find-file (car files))
    (goto-char (point-min))
    (query-replace symbol replacement)
    (save-buffer)
    (replace/open-and-replace-in-files (cdr files) symbol replacement)))

(defun replace/special-character? (char)
  "True if CHAR is not a word character."
  (or (< char 65)
      (and (> char 90) (< char 97))
      (> char 122)))

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
	       (file-list (find/get-files symbol)))
	  (replace/open-and-replace-in-files file-list symbol replacement)
	  (when old-buffer
	    (switch-to-buffer old-buffer)
            (goto-char old-point)))))))

(provide 'replace)
;;; replace.el ends here
