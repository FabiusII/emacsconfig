;;; package --- Summary
;;; Commentary:
;;; Text based search

;;; Code:
(setq lexical-binding t)

(load-file "/Users/fabianhanselmann/repos/emacsconfig/util.el")

(defun find/ns-of-file ()
  (let ((orig-point (point)))
    (goto-char (point-min))
    (forward-thing 'symbol 2)
    (let ((symbol (thing-at-point 'symbol)))
      (goto-char orig-point)
      symbol)))

(defun find/ns-name (var-name)
  (when (featurep 'cider)
    (require 'cider)
    (let ((orig-buffer (current-buffer))
          (_ (call-interactively 'cider-find-var var-name))
          (ns-name (find/ns-of-file)))
      (switch-to-buffer orig-buffer)
      ns-name)))

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

(defun find/get-filenames (ag-hit-list result)
  "Recursively parse lines from AG-HIT-LIST to filenames into RESULT ."
  (if (not ag-hit-list)
      result
    (let* ((next-hit (car ag-hit-list))
	   (file-name (car (split-string next-hit ":")))
	   (new-result (cons file-name result))
	   (new-result (delete-dups new-result)))
      (find/get-filenames (cdr ag-hit-list) new-result))))

(defvar import-regex
  (rx "(ns" (* anything) ":require" (+ (or "\s" "\n")) "[" (* anything) "%s" (+ (or "\s" "\n")) ":refer"
      (+ (or "\s" "\n")) "["(* anything) "%s" (* anything) "]"))

(defun find/refer-regex (ns-name var-name)
  (format import-regex ns-name var-name))

(defun find/namespace-imported? (file-content ns-name var-name)
  (string-match (find/refer-regex ns-name var-name) file-content))

(defun find/read-file-content (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-string)))

(defun find/ag-result-string->list (result-string ns-name var-name)
  "Turn a ag RESULT-STRING into a list of filenames."
  (let* ((lines (split-string result-string "\n"))
         (files (find/get-filenames lines '())))
    (seq-filter (lambda (file-name)
                  (find/namespace-imported?
                   (find/read-file-content file-name)
                   ns-name
                   var-name))
                (reverse files))))

(defun find-files (var)
  (let* ((ns-name (find/ns-name var))
         (search-string (format "\"%s\"" (find/escape-special-characters var)))
         (command (format "ag %s %s" search-string (projectile-project-root)))
         (result-string (shell-command-to-string command))
         (result-string (string-trim result-string)))
    (find/ag-result-string->list result-string ns-name var)))

(defun find-reference ()
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
         (file-list (find-files symbol)))
    (if (featurep 'ivy)
        (progn (require 'ivy)
               (ivy-read "References" file-list :action (lambda (e)
                                                          (-> (find-file-noselect e)
                                                              (pop-to-buffer-same-window)))))
      (let ((buffer (get-buffer-create "*References*")))
        (switch-to-buffer buffer)
        (find/insert-results (car file-list) (cdr file-list))))))

(provide 'find)
;;; find.el ends here
