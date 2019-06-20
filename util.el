;;; package --- Summary
;;; Commentary:
;;; Collection of utility functions

;;; Code:
(setq lexical-binding t)

(defun rename-local-symbol ()
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
	 (prompt (message "replace %s with: " symbol))
	 (replacement (read-string prompt))
	 (scope (bounds-of-thing-at-point 'defun)))
    (query-replace symbol replacement 'delimited (car scope) (cdr scope))))

(provide 'util)
;;; util.el ends here
