;; Package Settings
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" "9b35c097a5025d5da1c97dba45fed027e4fb92faecbd2f89c2a79d2d80975181" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d74fe1508cff43708fa2f97c4bf58d19f0e002b2e0c92bf958bf483113b7d89d" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (solarized-theme rainbow-delimiters flatland-theme which-key aggressive-indent yaml-mode scss-mode rvm robe web-mode groovy-mode company-tern xref-js2 ag js2-refactor js2-mode org magit evil flycheck-joker flycheck company-flx key-chord avy highlight-defined projectile clj-refactor expand-region company gruvbox-theme paredit cider clojure-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Custom Settings
(require 'clojure-mode)
(require 'company)
(require 'eldoc)
(require 'clj-refactor)
(require 'flycheck-joker)
(require 'ido)
(require 'js2-mode)
(require 'js2-refactor)
(require 'xref-js2)
(require 'ag)
(require 'company-tern)
(require 'rvm)
(require 'which-key)
;(load "~/repos/emacsconfig/fira-code-ligatures.el")

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.spec\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(set-frame-font "Fira Code 14")
(load-theme 'solarized-dark)
;(fira-code-mode)
(setq ring-bell-function 'ignore)

(setq exec-path (append exec-path '("/usr/local/bin" "/usr/local/Cellar/node/11.3.0_1/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/Cellar/node/11.3.0_1/bin"))

(setq ido-enable-flex-matching t)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (add-hook 'company-mode-hook (lambda ()
				 (add-to-list 'company-backends 'company-capf)))
  (company-flx-mode +1)
  (setq company-idle-delay 0.5))

(setq eldoc-idle-delay 0)
(setq eldoc-echo-area-use-multiline-p t)

(defun align-whitespace (start end)
  (interactive "r")
  (align-regexp start end "\\(\\s-*\\)\\s-" 1 0 t)
  ;;(indent-region start end)
)

;; GLOBAL MODES
(ido-mode t)
(ido-everywhere t)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook 'display-line-numbers-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'rvm-use-default)
(add-hook 'after-init-hook 'show-paren-mode)
(add-hook 'after-init-hook 'which-key-mode)
(add-hook 'after-init-hook 'rainbow-delimiters-mode)
(add-hook 'after-init-hook 'global-magit-file-mode)

(add-hook 'after-init-hook 'projectile-global-mode)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(key-chord-define-global "fj" 'avy-goto-word-1)
(key-chord-define-global "jf" 'avy-goto-word-1)
(key-chord-mode 1)

;; ELISP and IELM MODES
(add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'ielm-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; CLOJURE and CIDER MODES
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)

(defun clj-refactor-hooks ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'clj-refactor-hooks)

(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (wrap-result 'defun)
     (handle-error 'defun)
     (context-defn 'defun)
     (context-fn 'defun)
     (context-fn-stub 'defun)
     (defroutes 'defun)
     (testing 'defun)
     (feature 'defun)
     (scenario 'defun)
     (cond-let 'defun)
     (if-with-open 'defun)
     (when-let* 'defun)
     (fnk 'defun)
     (fact 'defun)
     (facts 'defun)
     (conditional 'defun)
     (wrap-clean-transaction 'defun)
     (GET 2)
     (POST 2)
     (PUT 2)
     (DELETE 2)
     (HEAD 2)
     (ANY 2)
     (context 2)))


;; JS Modes
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
(define-key js2-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
			   (tern-mode)
			   (add-to-list 'company-backends 'company-tern)
			   (add-hook 'xref-backend-functions #'xref-js2-xref-backend)))
(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)
(add-hook 'js2-mode-hook 'electric-pair-mode)
(add-hook 'js2-mode-hook 'aggressive-indent-mode)

;; Ruby modes
(add-hook 'ruby-mode-hook (lambda ()
			    (robe-mode)
			    (add-to-list 'company-backends 'company-robe)
			    (electric-pair-mode)
			    (aggressive-indent-mode)))

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;; Webmodes
(add-hook 'web-mode-hook (lambda ()
			   (electric-pair-mode)
			   (aggressive-indent-mode)))

;; Groovy
(add-hook 'groovy-mode-hook (lambda ()
			      (electric-pair-mode)
			      (aggressive-indent-mode)))

;; CUSTOM KEY BINDINGS
(global-set-key (kbd "S-SPC") 'company-complete)
(global-set-key (kbd "C-;") 'er/expand-region)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "M-1") 'windmove-left)
(global-set-key (kbd "M-2") 'windmove-right)

(global-hl-line-mode +1)
(setq mac-command-modifier 'meta)
(setq org-log-done t)
(provide '.emacs)
;;; .emacs.el ends here
(put 'upcase-region 'disabled nil)
