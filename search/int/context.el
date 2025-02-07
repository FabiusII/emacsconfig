(load-file "~/repos/emacsconfig/search/business/util.el")

(defun int/read-file-content (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-string)))

(defun int/grep-project (regex)
  (-> (format "ag %s %s" regex (projectile-project-root))
      (shell-command-to-string)
      (string-trim)
      (split-string "\n")
      (reverse)))

(defun int/ns-name (var-name)
  (when (featurep 'cider)
    (require 'cider)
    (let ((orig-buffer (current-buffer))
          (_ (call-interactively 'cider-find-var var-name))
          (ns-name (find/ns-of-file)))
      (switch-to-buffer orig-buffer)
      ns-name)))

(defvar int-context
  (list
   'read-file-content #'int/read-file-content
   'grep-project #'int/grep-project
   'ns-name #'int/ns-name
   ))
