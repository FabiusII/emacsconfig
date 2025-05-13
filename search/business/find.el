;;; package --- Summary
;;; Commentary:
;;; Text based search

;;; Code:
(setq lexical-binding t)

(load-file "~/repos/emacsconfig/search/business/util.el")
(require 'cl-lib)

(defun find/ns-of-file ()
  (let ((orig-point (point)))
    (goto-char (point-min))
    (forward-thing 'symbol 2)
    (let ((symbol (thing-at-point 'symbol)))
      (goto-char orig-point)
      symbol)))

(defun find/insert-results (current remaining)
  (when current
    (insert current)
    (insert "\n")
    (find/insert-results (car remaining) (cdr remaining))))

(defun find/special-character? (char)
  "True if CHAR is not a word character."
  (or (< char 65)
      (and (> char 90) (< char 97))
      (> char 122)))

(defun find/escape-special-characters (str &optional result idx)
  "Escape all characters with backslash that are not word characters.
Required by `ag`
STR the string to escape.
RESULT The result accumulator for recursion.
IDX the next character in STR"
  (let ((idx (or idx 0))
        (result (or result "")))
    (if (> (length str) idx)
        (let ((ascii (aref str idx)))
          (if (find/special-character? ascii)
              (find/escape-special-characters str (concat result (string 92 ascii)) (+ idx 1))
            (find/escape-special-characters str (concat result (string ascii)) (+ idx 1))))
      result)))

(defun find/file-names-and-line-numbers (ag-hit-list result)
  "Recursively parse lines from AG-HIT-LIST to filenames into RESULT ."
  (if (not ag-hit-list)
      result
    (let* ((next-hit (car ag-hit-list))
           (name-and-line (split-string next-hit ":"))
	   (file-name (car name-and-line))
           (line-number (--> name-and-line (cdr) (car) (string-to-number)))
           (existing-line-numbers (alist-get file-name result nil nil 'equal))
           (line-numbers (cons line-number existing-line-numbers))
           (new-result (cons
                        (cons file-name line-numbers)
                        (assoc-delete-all file-name result))))
      (find/file-names-and-line-numbers (cdr ag-hit-list) new-result))))

(defvar require-refer-regex
  (rx "(ns" (* anything) ":require" (+ (or "\s" "\n")) "[" (* anything) "%s" (+ (or "\s" "\n")) ":refer"
      (+ (or "\s" "\n")) "["(* anything) "%s" (* anything) "]"))

(defvar refer-all-regex
  (rx "(ns" (* anything) ":require" (+ (or "\s" "\n")) "[" (* anything) "%s" (+ (or "\s" "\n")) ":refer :all"))

(defvar require-as-regex
  "(ns\\(?:.\\|\n\\)*:require[\n ]+\\[\\(?:.\\|\n\\)*%s[\n ]+:as \\([a-zA-Z\-\.]+\\)")

(defun find/refer-regex (ns-name var-name)
  (format require-refer-regex ns-name var-name))

(defun find/refer-all-regex (ns-name)
  (format refer-all-regex ns-name))

(defun find/require-as-regex (ns-name)
  (format require-as-regex ns-name))

(defun find/namespace-imported? (file-content ns-name var-name)
  (or (string-match (find/refer-regex ns-name var-name) file-content)
      (string-match (find/refer-all-regex ns-name) file-content)))

(defun find/namespace-aliased? (file-content ns-name)
  (when (string-match (find/require-as-regex ns-name) file-content)
    (match-string 1 file-content)))

(defun find/aliased-usages (context files-and-lines ns-name var-name)
  (map-apply
   (lambda (file-name hit-lines)
     (let ((file-content (funcall (plist-get context 'read-file-content) file-name)))
       (when-let (alias (find/namespace-aliased? file-content ns-name))
         (let* ((aliased-name (format "%s/%s" alias var-name))
                (file-lines (split-string file-content "\n"))
                (actual-hits (seq-filter
                              (lambda (line-number)
                                (string-match aliased-name (nth (- line-number 1) file-lines)))
                              hit-lines)))
           (when actual-hits
             (cons file-name actual-hits))))))
   files-and-lines))

(defun find/imported-usages (context files-and-lines ns-name var-name)
  (map-filter (lambda (file-name hit-lines)
                (find/namespace-imported?
                 (funcall (plist-get context 'read-file-content) file-name)
                 ns-name
                 var-name))
              files-and-lines))

(defun find/hash-table->list (hash-table)
  (-->> (map-apply (lambda (file lines)
                     (seq-map (lambda (line)
                                (format "%s:%s" file line))
                              lines))
                   hash-table)
       (mapcan 'identity)))

(defun find/ag-result-string->list (context ag-hits ns-name var-name)
  "Turn a AG-HITS list into a list of filenames."
  (let* ((lines-in-importing-files (find/imported-usages context ag-hits ns-name var-name))
         (lines-in-aliasing-files (find/aliased-usages context ag-hits ns-name var-name)))
    (--> (map-merge 'hash-table lines-in-importing-files lines-in-aliasing-files)
         (find/hash-table->list))))

(defun find/references (context var)
  (let* ((ns-name (funcall (plist-get context 'ns-name ) var))
         (search-string (format "\"([a-zA-Z.-]+/)?%s\"" (find/escape-special-characters var)))
         (hit-string (funcall (plist-get context 'grep-project) search-string))
         (ag-hits (find/file-names-and-line-numbers hit-string  nil)))
    (find/ag-result-string->list context ag-hits ns-name var)))

(defun find/find-references (context)
  (let* ((symbol (thing-at-point 'symbol))
         (refs (find/references context symbol)))
    (if (featurep 'ivy)
        (progn (require 'ivy)
               (ivy-read "References" refs :action
                         (lambda (ref)
                           (let* ((file-and-line (split-string ref ":"))
                                  (file (car file-and-line))
                                  (line (car (cdr file-and-line)))
                                  (buffer (find-file-noselect file)))
                             (pop-to-buffer-same-window buffer)
                             (goto-char 0)
                             (forward-line (- (string-to-number line) 1))))))
      (let ((buffer (get-buffer-create "*References*")))
        (switch-to-buffer buffer)
        (find/insert-results (car refs) (cdr refs))))))

(provide 'find)
;;; find.el ends here
