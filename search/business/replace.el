;;; package --- Summary
;;; Commentary:
;;; Text based search and replace

;;; Code:
(setq lexical-binding t)

(load-file "/Users/fabianhanselmann/repos/emacsconfig/search/business/find.el")

(defun replace/rename-local-symbol (context)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (let* ((prompt (format "replace %s with: " symbol))
	     (replacement (read-string prompt))
	     (scope (bounds-of-thing-at-point 'defun)))
	(query-replace (regexp-quote symbol) replacement 'delimited (car scope) (cdr scope))))))

(defun replace/rename-in-buffer (context)
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

(defun replace/rename-in-project (context)
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
	       (file-list (find-files symbol)))
	  (replace/open-and-replace-in-files file-list symbol replacement)
	  (when old-buffer
	    (switch-to-buffer old-buffer)
            (goto-char old-point)))))))

(provide 'replace)
;;; replace.el ends here
