;; Package Settings
(require 'package)

(defmacro comment (&rest sexp) nil)

(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)
(add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/"))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(column-number-mode t)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(package-selected-packages
   '(lsp-ui lsp-metals lsp-mode scala-mode gh-md copilot copilot-chat ace-window highlight-defined backward-forward telephone-line super-save graphql-mode clomacs darcula-theme enh-ruby-mode use-package company-lsp markdown-mode rubocop ruby-electric ruby-test-mode flycheck-clj-kondo json-mode kibit-helper amx ivy all-the-icons-dired flatland-theme which-key yaml-mode scss-mode ag org magit flycheck company-flx key-chord avy projectile clj-refactor expand-region company gruvbox-theme paredit cider clojure-mode))
 '(safe-local-variable-values
   '((cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

(setq ring-bell-function 'ignore)
(setq visible-bell 1)
(setq ;use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(add-to-list 'custom-theme-load-path "~/repos/emacsconfig/themes/")

(eval-when-compile
  (require 'use-package))

;; Custom Settings
(setq-default indent-tabs-mode nil)

(use-package swiper)
(use-package counsel)
(use-package ag)
(use-package expand-region)
(use-package avy)

(use-package backward-forward
  :config
  (backward-forward-mode t)
  (global-set-key (kbd "M-[") 'backward-forward-previous-location)
  (global-set-key (kbd "M-]") 'backward-forward-next-location))

(use-package scss-mode
  :config
  (setq-default css-indent-offset 2))

(use-package magit
  :config
  (define-key magit-status-mode-map (kbd "M-1") nil)
  (define-key magit-status-mode-map (kbd "M-2") nil)
  (define-key magit-status-mode-map (kbd "M-3") nil)
  (define-key magit-status-mode-map (kbd "M-4") nil))

(use-package super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil))

(use-package telephone-line
  :config (telephone-line-mode 1))

(use-package exec-path-from-shell)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package use-package-ensure-system-package)

(use-package company
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
  (setq company-lsp-provider :capf)
  (global-company-mode))

(use-package clojure-mode
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
  (put-clojure-indent 'cond-let-> 'defun)
  (put-clojure-indent 'GET 2)
  (put-clojure-indent 'POST 2)
  (put-clojure-indent 'PUT 2)
  (put-clojure-indent 'DELETE 2)
  (put-clojure-indent 'context 2)
  (add-hook 'clojure-mode-hook (lambda () (yas-minor-mode 1))))

(use-package key-chord
  :config
  (key-chord-define-global "fj" 'avy-goto-word-1)
  (key-chord-define-global "jf" 'avy-goto-word-1)
  (setq key-chord-safety-interval-forward 0.1)
  (key-chord-mode 1))

(use-package paredit
  :commands enable-paredit-mode
  :hook
  ((cider-repl-mode
    clojure-mode
    emacs-lisp-mode
    ielm-mode
    eshell-mode) . enable-paredit-mode))

(use-package cider
  :config
  (setq cider-dynamic-indentation nil)
  (setq cider-clojure-cli-parameters "-A:dev")
  (global-set-key (kbd "C-c f u") 'find-reference)
  (global-set-key (kbd "C-c C-e") 'cider-repl-clear-buffer))

(use-package clj-refactor
  :hook ((clojure-mode) . clj-refactor-mode)
  :config
  (global-set-key (kbd "C-c n c") 'cljr-clean-ns)
  (global-set-key (kbd "C-c n a") 'cljr-add-missing-libspec))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package flycheck-clj-kondo
  :ensure-system-package
  (clj-kondo . "brew install borkdude/brew/clj-kondo"))

(use-package eldoc
  :hook ((clojure-mode
          emacs-lisp-mode
          ielm-mode
          eshell-mode
          cider-repl-mode) . turn-on-eldoc-mode)
  :config
  (setq eldoc-idle-delay 0)
  (setq eldoc-echo-area-use-multiline-p t))

(use-package which-key
  :config
  (which-key-mode))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist '((swiper . ivy--regex)
	                        (counsel-ag . ivy--regex-plus)
	                        (t . ivy--regex-fuzzy)))
  (ivy-add-actions 'ivy-switch-buffer '(("k" kill-buffer "kill")))
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "M-v") 'yank)
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-k") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c k") 'projectile-ag)
  (global-set-key (kbd "C-S-s") 'counsel-ag)
  (global-set-key (kbd "C-c i") 'ivy-resume)
  (global-set-key (kbd "C-c m") 'counsel-imenu))

(use-package projectile
  :config
  (add-hook 'after-init-hook 'projectile-global-mode)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (global-set-key (kbd "M-t") 'projectile-find-file)
  (when (featurep 'ivy)
    (setq projectile-completion-system 'ivy)))

(use-package scss-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode)))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package all-the-icons)
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package copilot)
(use-package copilot-chat)

(use-package scala-mode :interpreter ("scala" . scala-mode))

(use-package sbt-mode :commands sbt-start sbt-command
  :config
  (substitute-key-definition 'minibuffer-complete-word
                             'self-insert-command
                             minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  ;; You could also swap out lsp for lsp-deffered in order to defer loading
  :hook  (scala-mode . lsp)
         (lsp-mode . lsp-lens-mode)
  :config
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  ;; (setq gc-cons-threshold 100000000) ;; 100mb
  ;; (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;; (setq lsp-idle-delay 0.500)
  ;; (setq lsp-log-io nil)
  ;; (setq lsp-completion-provider :capf)
  (setq lsp-prefer-flymake nil)
  ;; Makes LSP shutdown the metals server when all buffers in the project are closed.
  ;; https://emacs-lsp.github.io/lsp-mode/page/settings/mode/#lsp-keep-workspace-alive
  (setq lsp-keep-workspace-alive nil))

(use-package lsp-metals)
(use-package lsp-ui)
(use-package yasnippet)
(use-package posframe)
(use-package dap-mode
  :pin melpa-stable
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(tool-bar-mode 0)

(add-to-list 'default-frame-alist
             '(font . "Inconsolata 17"))

(set-frame-font "Inconsolata 17")

(load-theme 'gruvbox-light-hard t)
;(load-theme 'gruvbox-dark-soft t)
(scroll-bar-mode -1)

(global-hl-line-mode +1)
(global-so-long-mode 1)

;; GLOBAL MODES

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook 'display-line-numbers-mode)
(add-hook 'after-init-hook 'show-paren-mode)

;; ELISP, ESHELL and IELM MODES
(add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
(define-key emacs-lisp-mode-map (kbd "C-c C-k") (lambda ()
                                                    (interactive)
                                                    (message "evaluating buffer")
                                                    (eval-buffer)
                                                    (message "buffer evaluated")))
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)

;; CUSTOM KEY BINDINGS
(global-set-key (kbd "C-;") 'er/expand-region)
(global-set-key (kbd "C-w") 'er/expand-region)
(global-set-key (kbd "C-S-w") 'er/contract-region)
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "M-1") 'windmove-left)
(global-set-key (kbd "M-2") 'windmove-right)

(global-set-key (kbd "M-z") 'undo)

(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-x") 'kill-region)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "C-M-v") 'yank-pop)

(global-set-key (kbd "C-`") 'scroll-up)
(global-set-key (kbd "M-`") 'scroll-down)
(global-set-key (kbd "M-~") 'tmm-menubar)

(global-set-key (kbd "C-<tab>") 'ivy-switch-buffer)

(defun save-all ()
    (interactive)
    (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

(setq mac-command-modifier 'meta)
(setq org-log-done t)
(setq split-height-threshold nil)
(setq split-width-threshold 160)

(load-file "~/repos/emacsconfig/search/business/replace.el")
(global-set-key (kbd "C-c r l") 'rename-local-symbol)
(global-set-key (kbd "C-c r b") 'rename-in-buffer)
(global-set-key (kbd "C-c r p") 'rename-in-project)

(load-file "~/repos/emacsconfig/eshellrc.el")

(put 'upcase-region 'disabled nil)

(provide '.emacs)
;;; .emacs.el ends here
