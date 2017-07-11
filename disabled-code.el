;; -*-no-byte-compile: t; -*-

;; Set the defalut browser:
(setq browse-url-browser-function 'browse-url-chromium)

;; Enable auto-complete for python:
(add-hook 'python-mode-hook 'auto-complete-mode)

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))
