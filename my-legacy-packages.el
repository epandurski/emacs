;; -*-no-byte-compile: t; -*-

;;; my-legacy-packages.el --- Ensure the legacy packages are installed
;;; and configured.
;;;
;;; The packages installed by this file are *probably* not needed
;;; anymore, or have a replacement which is better.

;;; Commentary:

;;; Code:

(require 'my-base-bindings)
(eval-when-compile
  (require 'use-package))


(use-package flycheck
  :disabled
  :ensure t
  :demand t
  :config (add-hook 'after-init-hook #'global-flycheck-mode)
  :bind ("C-e" . flycheck-display-error-at-point))


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
  :disabled
  :ensure t
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))


(use-package lsp-treemacs
  :disabled
  :ensure t
  :commands (lsp-treemacs-sync-mode))


(use-package lsp-mode
  :disabled
  :ensure t
  :commands (lsp lsp-deferred)
  :custom (lsp-keymap-prefix "C-v")
  :config (lsp-treemacs-sync-mode 1)
  :hook
  (svelte-mode . lsp-deferred)
  (python-mode . lsp-deferred)
  (typescript-mode . lsp-deferred))


(provide 'my-legacy-packages)

;;; my-legacy-packages.el ends here
