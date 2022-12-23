;;; my-base-packages.el --- Ensure the most basic packages are
;;; installed and configured.

;;; Commentary:

;;; Code:

(require 'my-util-funcs)
(eval-when-compile
  (require 'use-package))


(use-package key-chord
  :ensure t
  :demand t
  :config
  (setq key-chord-two-keys-delay 0.15) ;; the default is 0.1
  (setq key-chord-one-key-delay 0.3) ;; the default is 0.2
  (key-chord-mode 1)
  ;; prefix for global commands:
  (key-chord-define-global "jk" my-commands-keymap)
  ;; prefix for mode-specific commands:
  (key-chord-define-global "fd" 'undefined))


(use-package which-key
  :ensure t
  :demand t
  :config (which-key-mode))


(use-package flx-ido
  :ensure t
  :commands (flx-ido-mode))


(use-package ido
  :ensure t
  :demand t
  :custom
  (ido-case-fold t)
  (ido-use-virtual-buffers t)
  (ido-ignore-extensions t "Ignore extensions in `completion-ignored-extensions`.")
  (ido-enable-flex-matching t)
  (ido-file-extensions-order '(
     ".org" ".txt" ".py" ".js" ".ts" ".rs" ".c" ".json" ".md" ".rst"
     ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
  (ido-auto-merge-work-directories-length -1 "Disable auto-merge.")
  (ido-use-faces t)
  :config
  (require 'my-base-bindings)
  (ido-mode 'both)
  (flx-ido-mode 1)
  (my-ido-mode-keys)
  :bind ("M-m" . ido-switch-buffer))


(use-package projectile
  :ensure t
  :demand t
  :custom (projectile-switch-project-action 'projectile-dired)
  :config (add-hook 'after-init-hook #'projectile-mode)
  :bind ("C-f" . projectile-commander))


(use-package flycheck
  :ensure t
  :demand t
  :config (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind ("C-e" . flycheck-display-error-at-point))


(use-package company
  :ensure t
  :demand t
  :custom (company-idle-delay nil "Disable idle completion.")
  :config (add-hook 'after-init-hook #'global-company-mode)
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


; NOTE: Run "pip install -U 'python-lsp-server'" to install the pylsp
; server for python.
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


(provide 'my-base-packages)

;;; my-base-packages.el ends here
