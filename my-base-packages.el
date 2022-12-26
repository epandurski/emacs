;; -*-no-byte-compile: t; -*-

;;; my-base-packages.el --- Ensure the most basic packages are
;;; installed and configured.

;;; Commentary:

;;; Code:

(require 'my-base-bindings)
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
  (ido-mode 'both)
  (flx-ido-mode 1)
  :bind (("M-m" . ido-switch-buffer)
         :map ido-common-completion-map
         ;; Use "M-j", "M-l" for switching between the matching
         ;; items. Also, do not activate isearch here ("M-y", "M-Y").
         ("M-l" . ido-next-match)
         ("M-j" . ido-prev-match)
         ("M-y" . undefined)
         ("M-Y" . undefined)
         :map ido-file-completion-map
         ;; Make sure "M-s" does what it is supposed to do. Ido binds
         ;; "M-s" to `ido-merge-work-directories`, but we will use
         ;; "M-y" for this.
         ("M-s" . other-window)
         ;; When finding files, `ido` overrides many of the navigation keys.
         ;; For example, "C-e" enters edit mode, "C-k" deletes the current
         ;; file, "M-m" creates a new directory. Here we bring back those
         ;; bindings that we really need.
         ("M-d" . delete-backward-char)
         ("M-e" . backward-kill-word)
         ("M-l" . ido-next-match)
         ("M-j" . ido-prev-match)
         ("M-i" . ido-prev-work-directory)
         ("M-k" . ido-next-work-directory)
         ("M-b" . toggle-input-method)
         ("M-v" . yank)
         ("M-w" . ido-forget-work-directory)
         ("M-f" . ido-wide-find-file-or-pop-dir)
         ("M-r" . ido-wide-find-dir-or-delete-dir)
         ("M-y" . ido-merge-work-directories)
         ("M-Y" . ido-merge-work-directories)
         ("C-y" . ido-merge-work-directories)
         ("C-o" . ido-fallback-command)
         :map ido-buffer-completion-map
         ;; Use "C-o" to enter `ido-find-file` mode from `ido-switch-buffer`
         ;; mode ("C-f" does this too). Press "C-o" again to fallback to the
         ;; classic `find-file` mode.
         ("C-o" . ido-enter-find-file)
         ;; Press "M-m" again to fallback to the classic `switch-to-buffer`.
         ("M-m" . ido-fallback-command)))


(use-package projectile
  :ensure t
  :demand t
  :custom (projectile-switch-project-action 'projectile-dired)
  :config (add-hook 'after-init-hook #'projectile-mode)
  :bind ("C-f" . projectile-commander))


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


(use-package flymake
  :commands (flymake-mode)
  :bind (:map my-commands-keymap
         ("f f" . flymake-show-buffer-diagnostics)
         ("f p" . flymake-show-project-diagnostics)
         :map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error))
  :hook (emacs-lisp-mode
         python-mode
         js-mode
         typescript-mode
         svelte-mode))


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


(use-package lsp-treemacs
  :ensure t
  :commands (lsp-treemacs-sync-mode))


(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom (lsp-keymap-prefix "C-v")
  :config (lsp-treemacs-sync-mode 1)
  :hook
  (svelte-mode . lsp-deferred)
  (python-mode . lsp-deferred)
  (typescript-mode . lsp-deferred))

; NOTE: Run "pip install -U 'python-lsp-server'" to install the pylsp
; server for python.


(provide 'my-base-packages)

;;; my-base-packages.el ends here
