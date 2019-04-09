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
    ("d74fe1508cff43708fa2f97c4bf58d19f0e002b2e0c92bf958bf483113b7d89d" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (which-key aggressive-indent yaml-mode scss-mode rvm robe web-mode groovy-mode company-tern xref-js2 ag js2-refactor js2-mode org magit evil flycheck-joker flycheck company-flx key-chord avy highlight-defined projectile clj-refactor expand-region company gruvbox-theme paredit cider clojure-mode))))
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

(rvm-use-default)
(show-paren-mode)
(which-key-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.spec\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(set-frame-font "Monaco 13")
(load-theme 'gruvbox)
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

(global-set-key (kbd "C-x g") 'magit-status)

(global-hl-line-mode +1)
(setq mac-command-modifier 'meta)
(setq org-log-done t)
(provide '.emacs)
;;; .emacs.el ends here
(put 'upcase-region 'disabled nil)
