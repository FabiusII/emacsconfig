(require 'cl-lib)

(defmacro ->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

(defmacro -> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
                           (cdr form))))))

(defun ->alist (cons-list)
  "Takes a list of cons cells and turns it into a hash-table"
  (cl-reduce (lambda (acc curr)
                 (if curr
                     (let* ((key (car curr))
                            (existing (plist-get acc key))
                            (value (cdr curr)))
                       (plist-put (append value existing) acc key))
                   acc))
               cons-list
               :initial-value '()))

(defun constantly (value)
  (lambda (& _) value))
