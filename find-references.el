(setq lexical-binding t)

(load-file "./find-replace.el")

(defun find-references/ns-name ()
  (let ((orig-point (point)))
    (goto-char (point-min))
    (forward-thing 'symbol 2)
    (let ((symbol (thing-at-point 'symbol)))
      (goto-char orig-point)
      symbol)))

(defun find-references/insert-results (current remaining)
  (when current
    (insert current)
    (insert "\n")
    (find-references/insert-results (car remaining) (cdr remaining))))

(defun find-reference ()
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
         (ns-name (find-references/ns-name))
         (command (format "ag %s %s" (format "\"%s\"" (find-replace/escape-special-characters symbol)) (projectile-project-root)))
         (result-string (shell-command-to-string command))
         (result-string (string-trim result-string))
         (file-list (find-replace/ag-result-string->list result-string))
         (buffer (get-buffer-create "*References*")))
    (switch-to-buffer buffer)
    (find-references/insert-results (car file-list) (cdr file-list))))

(provide 'find-references)
;;; find-references.el ends here
