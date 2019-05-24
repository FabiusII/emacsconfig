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
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "a8c210aa94c4eae642a34aaf1c5c0552855dfca2153fa6dd23f3031ce19453d4" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "9954ed41d89d2dcf601c8e7499b6bb2778180bfcaeb7cdfc648078b8e05348c6" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "450f3382907de50be905ae8a242ecede05ea9b858a8ed3cc8d1fbdf2d57090af" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" "9b35c097a5025d5da1c97dba45fed027e4fb92faecbd2f89c2a79d2d80975181" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d74fe1508cff43708fa2f97c4bf58d19f0e002b2e0c92bf958bf483113b7d89d" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (amx counsel ivy doom-modeline all-the-icons-dired doom-themes sublime-themes twilight-theme solarized-theme rainbow-delimiters flatland-theme which-key aggressive-indent yaml-mode scss-mode rvm robe web-mode groovy-mode company-tern xref-js2 ag js2-refactor js2-mode org magit evil flycheck-joker flycheck company-flx key-chord avy highlight-defined projectile clj-refactor expand-region company gruvbox-theme paredit cider clojure-mode))))
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
(require 'doom-themes)
(require 'doom-modeline)
(require 'magit)
(require 'projectile)
(require 'ivy)
(require 'swiper)
(require 'counsel)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.spec\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(tool-bar-mode 0)
(set-frame-font "Fira Code 14")

(doom-modeline-mode 1)

(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

(load-theme 'doom-molokai)
(set-face-attribute 'hl-line nil :inherit nil :background "#333333")
;; (fira-code-mode)
;; (setq ring-bell-function 'ignore)
(doom-themes-visual-bell-config)
(doom-themes-org-config)

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

(with-eval-after-load 'magit-status
  (define-key magit-status-mode-map (kbd "M-1") nil)
  (define-key magit-status-mode-map (kbd "M-2") nil)
  (define-key magit-status-mode-map (kbd "M-3") nil)
  (define-key magit-status-mode-map (kbd "M-4") nil))

(setq eldoc-idle-delay 0)
(setq eldoc-echo-area-use-multiline-p t)

;; GLOBAL MODES

;; (ido-mode t)
;; (ido-everywhere t)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook 'display-line-numbers-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'rvm-use-default)
(add-hook 'after-init-hook 'show-paren-mode)
(add-hook 'after-init-hook 'which-key-mode)
(add-hook 'after-init-hook 'rainbow-delimiters-mode)
(add-hook 'after-init-hook 'global-magit-file-mode)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(add-hook 'after-init-hook 'projectile-global-mode)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(when (featurep 'ivy)
  (setq projectile-completion-system 'ivy))

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

(add-hook 'clojure-mode-hook
	  (lambda ()
	    (clj-refactor-mode 1)
	    (yas-minor-mode 1)
	    (cljr-add-keybindings-with-prefix "C-c C-m")))

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

;; ivy bindings
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-c i") 'ivy-resume)
(global-set-key (kbd "C-c m") 'counsel-imenu)

(setq mac-command-modifier 'meta)
(setq org-log-done t)
(setq split-height-threshold nil)
(setq split-width-threshold 160)
(provide '.emacs)
;;; .emacs.el ends here
(put 'upcase-region 'disabled nil)
