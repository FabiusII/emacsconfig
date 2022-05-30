;;; package --- Summary
;;; Commentary:
;;; Highlight user defined vars

;;; Code:
(setq lexical-binding t)

(require 'clomacs)

(clomacs-defun clj-all-ns all-ns)
(clomacs-defun clj-symbols ns-interns)
(clomacs-defun clj-ns-name ns-name)
(clomacs-def current-ns *ns*)

(defmacro quotable-namespace (namespace-name)
  namespace-name
  ;(clj-symbols ',namespace-name)
  )

(defun highlight/defined-in-ns (ns)
  (let ((name (clj-ns-name ns))
        (quoteable-name (quotable-namespace name)))
    (clj-symbols quotable-namespace)))

(defun highlight-defined ()
  (let ((all-symbols (mapcar (lambda (ns) (highlight/defined-in-ns ns)) (clj-all-ns))))
    (apply append all-symols)))

(provide 'highlight)
