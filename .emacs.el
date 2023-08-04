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
  (when (equal emacs-version "27.2")
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
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
 '(ansi-color-names-vector
   ["#3c3836" "#fb4934" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(column-number-mode t)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(package-selected-packages
   '(telephone-line super-save graphql-mode clomacs darcula-theme enh-ruby-mode use-package company-lsp markdown-mode rubocop ruby-electric ruby-test-mode flycheck-clj-kondo json-mode kibit-helper amx ivy all-the-icons-dired flatland-theme which-key yaml-mode scss-mode ag org magit flycheck company-flx key-chord avy highlight-defined projectile clj-refactor expand-region company gruvbox-theme paredit cider clojure-mode))
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

(add-to-list 'custom-theme-load-path "/Users/fabianhanselmann/repos/emacsconfig/themes/")

(eval-when-compile
  (require 'use-package))

;; Custom Settings
(setq-default indent-tabs-mode nil)

(use-package swiper :ensure t)
(use-package counsel :ensure t)
(use-package ag :ensure t)

(use-package magit
  :ensure t
  :config
  (define-key magit-status-mode-map (kbd "M-1") nil)
  (define-key magit-status-mode-map (kbd "M-2") nil)
  (define-key magit-status-mode-map (kbd "M-3") nil)
  (define-key magit-status-mode-map (kbd "M-4") nil))

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil))

(use-package telephone-line
  :ensure t
  :config (telephone-line-mode 1))

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
  (global-set-key (kbd "C-c f u") 'find-reference)
  (global-set-key (kbd "C-c C-o") 'cider-repl-clear-buffer))

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

(use-package ivy
  :ensure t
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
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-S-s") 'counsel-ag)
  (global-set-key (kbd "C-c i") 'ivy-resume)
  (global-set-key (kbd "C-c m") 'counsel-imenu))

(use-package projectile
  :ensure t
  :config
  (add-hook 'after-init-hook 'projectile-global-mode)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (global-set-key (kbd "M-t") 'projectile-find-file)
  (when (featurep 'ivy)
    (setq projectile-completion-system 'ivy)))

(use-package scss-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.sass\\'" . scss-mode)))

(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package all-the-icons :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(tool-bar-mode 0)

(add-to-list 'default-frame-alist
             '(font . "Inconsolata 17"))

(set-frame-font "Inconsolata 17")

;(load-theme 'gruvbox-light-hard t)
(load-theme 'gruvbox-dark-soft t)

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

(global-set-key (kbd "M-[") 'pop-global-mark)
(global-set-key (kbd "M-]") 'exchange-point-and-mark)

(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-Z") 'undo-tree-redo)

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
