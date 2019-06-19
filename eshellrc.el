;;; package --- Summary

;;; Commentary:
;;; eshell config

;;; Code:

(require 'em-term)

(add-to-list 'eshell-visual-commands "top")
(add-to-list 'eshell-visual-options "git" "--help")
(add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))

(setq eshell-destroy-buffer-when-process-dies t)

(provide 'eshellrc)
;;; eshellrc.el ends here


