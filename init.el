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
 '(cua-mode nil nil (cua-base))
 '(cua-remap-control-v nil)
 '(current-language-environment "UTF-8")
 '(default-input-method "bulgarian-phonetic")
 '(dired-listing-switches "-al --time-style=long-iso --group-directories-first")
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+$")
 '(dired-recursive-copies (quote top))
 '(dired-recursive-deletes (quote top))
 '(dired-use-ls-dired t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(global-subword-mode t)
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-strict-trailing-comma-warning nil)
 '(json-reformat:indent-width 2)
 '(rust-rustfmt-bin "~/.cargo/bin/rustfmt")
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

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

;; Comment/uncomment lines to enable/disable archives as desired:
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)



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
  (projectile-global-mode))


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
  :bind (
         :map python-mode-map
         ("M-7" . python-shell-switch-to-shell)
         ("C-," . python-indent-shift-left)
         ("C-." . python-indent-shift-right)
         ("M-U" . beginning-of-defun)
         ("M-O" . end-of-defun)
         :map inferior-python-mode-map
         ("M-r" . kill-word)
         ("C-r" . comint-history-isearch-backward-regexp)))


(use-package virtualenvwrapper
  :ensure t
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))


(use-package company-jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'my-company-jedi-configuration-hook))
(defun my-company-jedi-configuration-hook ()
  "My `company-jedi` initializations."
  (jedi:setup)
  (define-key python-mode-map (kbd "<f1>") 'jedi:show-doc)
  (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key python-mode-map (kbd "M->") 'jedi:goto-definition-pop-marker)
  (add-to-list 'company-backends 'company-jedi))


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
  :bind (
         ("C-x g" . magit-status)
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
  :init (global-flycheck-mode))


(use-package js2-mode
  :ensure t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (add-hook 'js2-mode-hook 'my-js2-mode-hook)
  :config
  (unbind-key "M-j" js2-mode-map)
  :bind (
         :map js2-mode-map
              ("M-7" . nodejs-repl)
              ("M-j" . backward-char)
              ("C-n" . js2-next-error)))
(defun my-js2-mode-hook ()
  "My `js2-mode` initializations."
  (electric-pair-mode 1))


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


(provide 'my-init)

;;; init.el ends here
