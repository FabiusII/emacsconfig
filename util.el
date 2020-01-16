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

(defun ->hash-table (cons-list)
  "Takes a list of cons cells and turns it into a hash-table"
  (cl-reduce (lambda (acc curr)
                 (if curr
                     (let* ((key (car curr))
                            (existing (gethash key acc))
                            (value (cdr curr)))
                       (a-assoc acc key (append value existing)))
                   acc))
               cons-list
               :initial-value (a-hash-table)))
