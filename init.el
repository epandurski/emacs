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
 '(dired-dwim-target 'dired-dwim-target-next)
 '(dired-isearch-filenames t)
 '(dired-listing-switches "-alh --time-style=long-iso --group-directories-first")
 '(dired-omit-files "^[#]\\|^[.]$\\|^[.]\\(?:[^.]\\|[.].+\\)")
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
 '(json-reformat:indent-width 2)
 '(scroll-bar-mode 'right)
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

(defvar my-commands-keymap (make-keymap "Custom commands")
  "Custom commands invoked with a key-chord.")

(add-to-list 'load-path "~/src/emacs")
(byte-recompile-directory "~/src/emacs" 0)
(defalias 'yes-or-no-p 'y-or-n-p)
(require 'jka-compr)
(require 'dired-x)
(require 'find-dired)
(setq find-ls-option
      '("-exec ls -ldhb --time-style=long-iso --group-directories-first {} +" .
        "-ldhb --time-style=long-iso --group-directories-first"))
(setq-default dired-omit-mode t)
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 100)
(require 'uniquify)
(require 'my-abbrevs)
(require 'my-base-bindings)
(require 'my-server)
(require 'my-workarounds)
(setq grep-program "egrep")
(setq compilation-scroll-output t)  ;; or 'first-error
(electric-pair-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure package archives ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;; Circumvent a bug in Emacs 26.1 (fixed in 27.1).
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Comment/uncomment lines to enable/disable archives as desired:
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("myelpa" . "~/myelpa/")))
(setq package-archive-priorities
      '(("myelpa" . 15)
        ("melpa-stable" . 10)
        ("gnu" . 5)
        ("melpa" . 0)))

(package-initialize)

;; Ensure packages in my-package-list are installed.
(setq my-package-list '(use-package bind-key diminish))
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; `elpa-mirror` is a module that creates a local Emacs package
;; repository from installed packages, so that package upgrade never
;; breaks (see https://github.com/redguardtoo/elpa-mirror). Use `M-x
;; elpamr-create-mirror-for-installed` to (re)create the local
;; repository.
(require 'elpa-mirror)

;; Configure dired file associations.
(setq dired-guess-shell-alist-user
      '(("\\.\\(mp3\\|ogg\\|wav\\)\\'" "audacious")
        ("\\.\\(mp4\\|webm\\|mov\\|avi\\)\\'" "vlc")))

;; Add code navigation commands to the Edit menu. This makes easier
;; navigating the code using only the mouse.
(easy-menu-add-item nil '("edit") ["--" nil t])
(easy-menu-add-item nil '("edit") ["Jump to Definition" xref-find-definitions t])
(easy-menu-add-item nil '("edit") ["Jump Back from Definition" xref-pop-marker-stack t])


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure use-package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)


(use-package key-chord
  :ensure t
  :init
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.15) ;; the default is 0.1
  (setq key-chord-one-key-delay 0.3) ;; the default is 0.2
  ;; prefix for global commands:
  (key-chord-define-global "jk" my-commands-keymap)
  ;; prefix for mode-specific commands:
  (key-chord-define-global "fd" 'undefined))


(use-package which-key
  :ensure t
  :init (which-key-mode))


(use-package flx-ido
  :ensure t
  :commands (flx-ido-mode))


(use-package ido
  :ensure t
  :init
  (ido-mode 'both)
  (flx-ido-mode 1)
  (add-to-list 'completion-ignored-extensions ".ignore_this_extension")
  (setq ido-case-fold t)
  (setq ido-use-virtual-buffers t)
  (setq ido-ignore-extensions t)
  (setq ido-enable-flex-matching t)
  (setq ido-file-extensions-order
        '(".org" ".txt" ".py" ".js" ".ts" ".rs" ".c" ".json" ".md" ".rst"
          ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
  (setq ido-auto-merge-work-directories-length -1) ;; disable auto-merge
  (setq ido-use-faces t)
  :config
  (my-ido-mode-keys)
  :bind
  ("M-m" . ido-switch-buffer))


(use-package projectile
  :ensure t
  :bind ("C-f" . projectile-commander)
  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (add-hook 'after-init-hook #'projectile-global-mode))


(use-package flycheck
  :ensure t
  :bind ("C-e" . flycheck-display-error-at-point)
  :init (global-flycheck-mode))


(use-package company
  :ensure t
  :init
  (setq company-idle-delay nil)
  (add-hook 'after-init-hook #'global-company-mode)
  :bind (("M-/" . company-complete-common-or-cycle)
         :map company-active-map
         ;; Company binds some keys that we may need. Unbind them, and
         ;; define our own.
         ("C-s" . nil)
         ("C-w" . nil)
         ("M-y" . company-search-candidates)
         ("M-." . company-show-location)))


(use-package magit
  :ensure t
  :commands (magit-status magit-file-dispatch)
  :bind (("C-x g" . magit-status) ;; an old habit, use "jk g" intead
         :map my-commands-keymap
         ("g" . magit-status)
         ("G" . magit-file-dispatch)
         :map magit-mode-map
         ;; Magit binds some keys that we need. Unbind them, and
         ;; define our own.
         ("M-1" . nil)
         ("M-2" . nil)
         ("M-3" . nil)
         ("M-4" . nil)
         ("M-w" . nil)
         ("C-w" . nil)
         ("1" . magit-section-show-level-1-all)
         ("2" . magit-section-show-level-2-all)
         ("3" . magit-section-show-level-3-all)
         ("4" . magit-section-show-level-4-all)
         ("M-c" . magit-copy-buffer-revision)
         ("M-x" . magit-copy-section-value)))


(use-package python
  :ensure t
  :commands (python-mode)
  :config
  (add-hook 'python-mode-hook #'hs-minor-mode)
  (add-hook 'python-mode-hook #'imenu-add-menubar-index)
  (add-hook 'python-mode-hook #'flyspell-prog-mode)
  :bind (:map python-mode-map
              ("M-7" . python-shell-switch-to-shell)
              ("M-9" . python-indent-shift-left)
              ("M-0" . python-indent-shift-right)
              ("M-U" . beginning-of-defun)
              ("M-O" . end-of-defun)
         :map inferior-python-mode-map
              ("M-r" . kill-word)
              ("C-r" . comint-history-isearch-backward-regexp)))


(use-package js
  :ensure t
  :commands (js-mode)
  :config
  (setq js-indent-level 2)
  (add-hook 'js-mode-hook #'flyspell-prog-mode))


(use-package typescript-mode
  :ensure t
  :commands (typescript-mode)
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook #'flyspell-prog-mode))


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  (add-hook 'markdown-mode-hook #'imenu-add-menubar-index)
  (setq markdown-enable-prefix-prompts nil)
  (setq markdown-asymmetric-header nil)
  (setq markdown-xhtml-header-content (concat
         "<style type=\"text/css\">"
         (my-file-to-string "~/src/emacs/splendor.css")
         "</style>"))
  (setq markdown-command (concat
         "cmark-gfm"
         " -e table"
         " -e tasklist"
         " -e strikethrough"
         " -e tagfilter"
         " -e autolink"
         " -e footnotes"))
  (setq my-markdown-mode-keymap (make-keymap))
  (key-chord-define markdown-mode-map "fd" my-markdown-mode-keymap)
  :bind (:map markdown-mode-map
         ("M-<return>" . markdown-insert-list-item)
         ("M-p" . markdown-outline-previous-same-level)
         ("M-n" . markdown-outline-next-same-level)
         ("M-P" . markdown-outline-previous)
         ("M-N" . markdown-outline-next)
         ("M-C-p" . markdown-move-up)
         ("M-C-n" . markdown-move-down)
         ("M-7" . markdown-outline-up)
         ("M-9" . markdown-promote)
         ("M-0" . markdown-demote)
         :map my-markdown-mode-keymap
         ("d" . markdown-do)
         ("h" . markdown-insert-header-dwim)
         ("i" . markdown-insert-italic)
         ("b" . markdown-insert-bold)
         ("q" . markdown-insert-blockquote)
         ("," . markdown-outdent-region)
         ("." . markdown-indent-region)
         ("p" . markdown-preview)
         ("e" . markdown-export)
         ("r" . markdown-check-refs)
         ("n" . markdown-cleanup-list-numbers)))


(use-package sgml-mode
  :ensure t
  :commands (html-mode sgml-mode)
  :config
  (setq sgml-validate-command "tidy --gnu-emacs yes -utf8 -e -q")
  :bind (:map html-mode-map
              ("M-7" . sgml-delete-tag)
              ("M-9" . sgml-tag)
              ("M-0" . sgml-close-tag)
              ("M-O" . sgml-skip-tag-forward)
              ("M-U" . sgml-skip-tag-backward)))


(use-package json-mode
  :ensure t
  :commands (json-mode)
  :mode "\\.json\\'")


(use-package yaml-mode
  :ensure t
  :commands (yaml-mode)
  :mode "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook #'flyspell-prog-mode))


(use-package jinja2-mode
  :commands (jinja2-mode)
  :ensure t)


(use-package dockerfile-mode
  :ensure t
  :commands (dockerfile-mode)
  :config
  (add-hook 'dockerfile-mode-hook #'flyspell-prog-mode))


(use-package svelte-mode
  :commands (svelte-mode)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages which are *probably* not needed anymore,  ;;
;; or have a replacement which is better.             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package nodejs-repl
  :disabled
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


(use-package add-node-modules-path
  :ensure t)


(use-package virtualenvwrapper
  :ensure t
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))



(provide 'my-init)

;;; init.el ends here
