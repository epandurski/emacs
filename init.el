;; -*-no-byte-compile: t; -*-

;;; Commentary:
;;;
;;; This is my main Emacs configuration file.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-remap-control-v nil)
 '(default-input-method "bulgarian-phonetic")
 '(dired-listing-switches "-al --time-style=long-iso --group-directories-first")
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$")
 '(dired-recursive-copies 'top)
 '(dired-recursive-deletes 'top)
 '(dired-use-ls-dired t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(fringe-mode '(nil . 0) nil (fringe))
 '(global-subword-mode t)
 '(help-at-pt-display-when-idle '(flymake-overlay) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(json-reformat:indent-width 2)
 '(rust-rustfmt-bin "~/.cargo/bin/rustfmt")
 '(scroll-bar-mode 'right)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(typescript-indent-level 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 128 :width normal))))
 '(mode-line ((((class color) (min-colors 88)) (:background "#9dbde4" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(region ((t (:background "LightGoldenrod2")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure build-in packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/src/emacs")
(byte-recompile-directory "~/src/emacs" 0)
(defalias 'yes-or-no-p 'y-or-n-p)
(require 'jka-compr)
(require 'dired-x)
(setq-default dired-omit-mode t)
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 100)
(require 'uniquify)
(require 'my-abbrevs)
(require 'my-base-bindings)
(require 'my-server)
(require 'my-workarounds)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure package archives ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;; Circumvent a bug in Emacs 26.1 (fixed in 27.1).
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Comment/uncomment lines to enable/disable archives as desired:
(setq package-archives
      '(("gnu"     . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")))
(setq package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu" . 5)
        ("melpa" . 0)))

(package-initialize)

;; Ensure packages in my-package-list are installed.
(setq my-package-list '(use-package))
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure use-package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)


(use-package flx-ido
  :ensure t
  :init
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))


(use-package projectile
  :ensure t
  :bind ("C-f" . projectile-commander)
  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (add-hook 'after-init-hook 'projectile-global-mode))


(use-package company
  :ensure t
  :bind ("M-/" . company-complete-common-or-cycle)
  :init
  (setq company-idle-delay nil)
  (add-hook 'after-init-hook 'global-company-mode))


(use-package python
  :ensure t
  :config
  (unbind-key "C-c <" python-mode-map)
  (unbind-key "C-c >" python-mode-map)
  :init
  (add-hook 'python-mode-hook 'hs-minor-mode)
  (add-hook 'python-mode-hook 'imenu-add-menubar-index)
  :bind (:map python-mode-map
              ("M-7" . python-shell-switch-to-shell)
              ("C-," . python-indent-shift-left)
              ("C-." . python-indent-shift-right)
              ("M-U" . beginning-of-defun)
              ("M-O" . end-of-defun)
              ("M-'" . hs-toggle-hiding)
              ("M-\"" . hs-hide-level)
              ("C-'" . hs-show-all)
         :map inferior-python-mode-map
              ("M-r" . kill-word)
              ("C-r" . comint-history-isearch-backward-regexp)))


(use-package virtualenvwrapper
  :ensure t
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))


(use-package sgml-mode
  :ensure t
  :bind (
         :map html-mode-map
              ("M-7" . sgml-tag)
              ("M-O" . sgml-skip-tag-forward)
              ("M-U" . sgml-skip-tag-backward)))


(use-package magit
  :ensure t
  :config
  (unbind-key "M-1" magit-mode-map)
  (unbind-key "M-2" magit-mode-map)
  (unbind-key "M-3" magit-mode-map)
  (unbind-key "M-4" magit-mode-map)
  (unbind-key "M-w" magit-mode-map)
  (unbind-key "C-w" magit-mode-map)
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-dispatch-popup)
         :map magit-mode-map
         ("1" . magit-section-show-level-1-all)
         ("2" . magit-section-show-level-2-all)
         ("3" . magit-section-show-level-3-all)
         ("4" . magit-section-show-level-4-all)
         ("M-c" . magit-copy-buffer-revision)
         ("M-x" . magit-copy-section-value)))


(use-package flycheck
  :ensure t
  :bind ("C-e" . flycheck-explain-error-at-point)
  :init (global-flycheck-mode))


(use-package js
  :ensure t
  :init
  (add-hook 'js-mode-hook 'my-js-mode-hook))
(defun my-js-mode-hook ()
  "My `js-mode` initializations."
  (electric-pair-mode 1))


(use-package nodejs-repl
  :ensure t
  :bind (:map js-mode-map
              ("M-7" . nodejs-repl-switch-to-repl)
              ("C-c C-c" . nodejs-repl-send-buffer)
              ("C-c C-e" . nodejs-repl-send-last-expression)
              ("C-c C-j" . nodejs-repl-send-line)
              ("C-c C-r" . nodejs-repl-send-region)
              ("C-c C-l" . nodejs-repl-load-file)
         :map nodejs-repl-mode-map
              ("M-r" . kill-word)
              ("C-r" . comint-history-isearch-backward-regexp)))


(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :init
  (add-hook 'json-mode-hook 'my-json-mode-hook))
(defun my-json-mode-hook ()
  "My `json-mode` initializations."
  (electric-pair-mode 1))


(use-package json-reformat
  :ensure t)


(use-package yaml-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))


(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'my-rust-mode-hook))
(defun my-rust-mode-hook ()
  "My `rust-mode` initializations."
  (electric-pair-mode 1)
  (cargo-minor-mode 1))


(use-package cargo
  :ensure t)


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))


(use-package dockerfile-mode
  :ensure t)


(use-package jinja2-mode
  :ensure t)


(use-package add-node-modules-path
  :ensure t)


(use-package typescript-mode
  :ensure t)


(use-package svelte-mode
  :ensure t)


; Run "pip install -U 'python-lsp-server'" to install the pylsp server
; for python.
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-v")
  (add-hook 'lsp-deferred #'svelte-mode)
  (add-hook 'lsp-deferred #'python-mode)
  (add-hook 'lsp-deferred #'typescript-mode))


(use-package lsp-treemacs
  :init (lsp-treemacs-sync-mode 1)
  :ensure t
  :commands lsp-treemacs-errors-list)


(use-package which-key
  :ensure t
  :config (which-key-mode))


(provide 'my-init)

;;; init.el ends here
