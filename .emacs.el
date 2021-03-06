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
    ("41c8c11f649ba2832347fe16fe85cf66dafe5213ff4d659182e25378f9cfc183" "80939b7f9ed52b3072ba06b0e9c158c4c16c3cc3c376acbf04adf67c16053635" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "6bc387a588201caf31151205e4e468f382ecc0b888bac98b2b525006f7cb3307" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "a8c210aa94c4eae642a34aaf1c5c0552855dfca2153fa6dd23f3031ce19453d4" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "9954ed41d89d2dcf601c8e7499b6bb2778180bfcaeb7cdfc648078b8e05348c6" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "43c808b039893c885bdeec885b4f7572141bd9392da7f0bd8d8346e02b2ec8da" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "450f3382907de50be905ae8a242ecede05ea9b858a8ed3cc8d1fbdf2d57090af" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" "9b35c097a5025d5da1c97dba45fed027e4fb92faecbd2f89c2a79d2d80975181" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d74fe1508cff43708fa2f97c4bf58d19f0e002b2e0c92bf958bf483113b7d89d" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (darcula-theme doom-themes enh-ruby-mode use-package company-lsp markdown-mode rubocop ruby-electric ruby-test-mode treemacs-projectile flycheck-clj-kondo json-mode kibit-helper amx ivy doom-modeline all-the-icons-dired sublime-themes twilight-theme solarized-theme rainbow-delimiters flatland-theme which-key aggressive-indent yaml-mode scss-mode rvm web-mode groovy-mode company-tern xref-js2 ag js2-refactor js2-mode org magit flycheck company-flx key-chord avy highlight-defined projectile clj-refactor expand-region company gruvbox-theme paredit cider clojure-mode)))
 '(safe-local-variable-values
   (quote
    ((cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'custom-theme-load-path "/Users/fabianhanselmann/repos/emacsconfig/themes/")

(eval-when-compile
  (require 'use-package))

;; Custom Settings
(require 'ag)
(require 'doom-modeline)
(require 'magit)
(require 'projectile)
(require 'ivy)
(require 'swiper)
(require 'counsel)

(setq-default indent-tabs-mode nil)

(use-package exec-path-from-shell
  :ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package company
  :ensure t
  :bind
  (("S-SPC" . 'company-complete)
   :map company-active-map
   ("M-n" . nil)
   ("M-p" . nil)
   ("C-n" . 'company-select-next)
   ("C-p" . 'company-select-previous))
  :config
  ;; (company-flx-mode +1)
  (setq company-idle-delay 0.2)
  (global-company-mode))

(use-package clojure-mode
  :ensure t
  :commands 'clojure-mode
  :config
  (put-clojure-indent 'wrap-result 'defun)
  (put-clojure-indent 'handle-error 'defun)
  (put-clojure-indent 'context-defn 'defun)
  (put-clojure-indent 'context-fn 'defun)
  (put-clojure-indent 'context-fn-stub 'defun)
  (put-clojure-indent 'defroutes 'defun)
  (put-clojure-indent 'testing 'defun)
  (put-clojure-indent 'conditional 'defun)
  (put-clojure-indent 'wrap-clean-transaction 'defun)
  (put-clojure-indent 'leave-clean-db 'defun)
  (put-clojure-indent 'GET 2)
  (put-clojure-indent 'POST 2)
  (put-clojure-indent 'PUT 2)
  (put-clojure-indent 'DELETE 2)
  (put-clojure-indent 'context 2))

(use-package paredit
  :ensure t
  :commands enable-paredit-mode
  :hook
  ((cider-repl-mode
    clojure-mode
    emacs-lisp-mode
    ielm-mode
    eshell-mode) . enable-paredit-mode))

(use-package cider
  :ensure t
  :config
  (global-set-key (kbd "C-c f u") 'find-reference))

(use-package clj-refactor
  :ensure t
  :hook ((clojure-mode) . clj-refactor-mode)
  :config
  (global-set-key (kbd "C-c n c") 'cljr-clean-ns)
  (global-set-key (kbd "C-c n a") 'cljr-add-missing-libspec))

(use-package flycheck-clj-kondo
  :ensure t
  :ensure-system-package
  (clj-kondo . "brew install borkdude/brew/clj-kondo"))

(use-package eldoc
  :ensure t
  :hook ((clojure-mode
          emacs-lisp-mode
          ielm-mode
          eshell-mode
          cider-repl-mode) . turn-on-eldoc-mode)
  :config
  (setq eldoc-idle-delay 0)
  (setq eldoc-echo-area-use-multiline-p t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package js2-mode
  :ensure t
  :commands js2-mode
  :bind
  (:map js2-mode-map
        ("C-k" . 'js2r-kill)
        ("M-." . nil))
  :config
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook 'electric-pair-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package js2-refactor
  :ensure t
  :hook ((js2-mode) . js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package tern
  :ensure t
  :bind
  (:map tern-mode-keymap
        ("M-." . nil)
        ("M-," . nil))
  :hook
  ((js2-mode) . tern-mode))

(use-package company-tern
  :ensure t
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package xref-js2
  :ensure t
  :config
  :hook ((js2-mode) . (add-hook 'xref-backend-functions #'xref-js2-xref-backend)))

(use-package aggressive-indent
  :ensure t
  :hook
  ((js2-mode
    enh-ruby-mode-hook
    groovy-mode
    web-mode) . aggressive-indent-mode))

(use-package rvm
  :ensure t
  :hook
  ((enh-ruby-mode) . rvm-activate-corresponding-ruby))

(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.spec\\'" . groovy-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

(tool-bar-mode 0)

(add-to-list 'default-frame-alist
             '(font . "Inconsolata 17"))

(set-frame-font "Inconsolata 17")

(doom-modeline-mode 1)
(load-theme 'vscode-default-dark t)
;(load-theme 'wilson)
(global-hl-line-mode +1)

(with-eval-after-load 'magit-status
  (define-key magit-status-mode-map (kbd "M-1") nil)
  (define-key magit-status-mode-map (kbd "M-2") nil)
  (define-key magit-status-mode-map (kbd "M-3") nil)
  (define-key magit-status-mode-map (kbd "M-4") nil))


;; (defun is-cljs-file (filename)
;;   (when filename
;;     (string-match "\\.cljs\\'" filename)))

;; (add-hook 'buffer-list-update-hook
;; 	  (lambda ()
;; 	    (if (is-cljs-file buffer-file-name)
;;                 (company-flx-mode -1)
;; 	      (company-flx-mode +1))))

;; GLOBAL MODES
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-re-builders-alist
      '((swiper . ivy--regex)
	(counsel-ag . ivy--regex-plus)
	(t . ivy--regex-fuzzy)))

(ivy-add-actions
 'ivy-switch-buffer
 '(("k" kill-buffer "kill")))

(define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook #'rvm-use-default)
(add-hook 'after-init-hook 'display-line-numbers-mode)
(add-hook 'after-init-hook 'show-paren-mode)
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

;; ELISP, ESHELL and IELM MODES
(add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") '(lambda ()
                                                   (interactive)
                                                   (message "evaluating buffer")
                                                   (eval-buffer)
                                                   (message "buffer evaluated")))
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)

(add-hook 'clojure-mode-hook
	  (lambda ()
	    (yas-minor-mode 1)))

;; Ruby modes
(add-hook 'enh-ruby-mode-hook
          (lambda ()
            (lsp)
            (push 'company-lsp company-backends)
            (ruby-electric-mode)
            (rubocop-mode)
            (ruby-test-mode)
            (global-set-key (kbd "M-RET") 'rubocop-autocorrect-current-file)))

;; Webmodes
(add-hook 'web-mode-hook 'electric-pair-mode)

;; Groovy
(add-hook 'groovy-mode-hook 'electric-pair-mode)

;; CUSTOM KEY BINDINGS
(global-set-key (kbd "C-;") 'er/expand-region)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "M-1") 'windmove-left)
(global-set-key (kbd "M-2") 'windmove-right)

(global-set-key (kbd "C-x M-b") 'pop-global-mark)

;; enable only when projectile is active
(when (featurep 'projectile)
  (global-set-key (kbd "M-t") 'projectile-find-file))

;; ivy bindings
(when (featurep 'ivy)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-c i") 'ivy-resume)
  (global-set-key (kbd "C-c m") 'counsel-imenu))

(setq mac-command-modifier 'meta)
(setq org-log-done t)
(setq split-height-threshold nil)
(setq split-width-threshold 160)

(load-file "~/repos/emacsconfig/replace.el")
(global-set-key (kbd "C-c r l") 'rename-local-symbol)
(global-set-key (kbd "C-c r b") 'rename-in-buffer)
(global-set-key (kbd "C-c r p") 'rename-in-project)

(load-file "~/repos/emacsconfig/eshellrc.el")

(put 'upcase-region 'disabled nil)

;;; TODO
;;; jump to references in references search buffer on click
;;; line search forward and backward with continuation

(provide '.emacs)
;;; .emacs.el ends here
