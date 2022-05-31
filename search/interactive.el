(setq lexical-binding t)

(load-file "./business/replace.el")
(load-file "./int/context.el")
(load-file "./business/find.el")

(defun rename-local-symbol ()
  "Renaming a symbol in the current function based on its text content alone.
Before renaming each single occurance, the user is asked for permission.
Only the user can detect if the text is actually a reference to the symbol."
  (interactive)
  (replace/rename-local-symbol int/context))

(defun rename-in-buffer ()
  "Renaming a symbol in the current buffer based on its text content alone.
Before renaming each single occurance, the user is asked for permission.
Only the user can detect if the text is actually a reference to the symbol."
  (interactive)
  (replace/rename-in-buffer int/context))


(defun rename-in-project ()
  "Renaming a symbol in the current buffer based on its text content alone.
Before renaming each single occurance, the user is asked for permission.
Only the user can detect if the text is actually a reference to the symbol.
Requies and projectile and ag (aka the_silver_searcher).
The latter must be installed on your system, not necessarily EMACS."
  (interactive)
  (replace/rename-in-project int/context))

(defun find-reference ()
  (interactive)
  (find/find-references int/context))
