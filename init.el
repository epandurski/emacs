;; -*-no-byte-compile: t; -*-

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
 '(global-flycheck-mode t)
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
 '(python-shell-interpreter "python3")
 '(rust-rustfmt-bin "~/.cargo/bin/rustfmt")
 '(safe-local-variable-values
   (quote
    ((test-case-name . twisted\.test\.test_abstract)
     (test-case-name . twisted\.test\.test_fdesc)
     (test-case-name . twisted\.internet\.test\.test_inotify)
     (test-case-name . twisted\.test\.test_udp))))
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


(add-to-list 'load-path "~/src/emacs")
(byte-recompile-directory "~/src/emacs" 0)
(defalias 'yes-or-no-p 'y-or-n-p)
(require 'jka-compr)
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(require 'my-abbrevs)
(require 'my-base-bindings)
(require 'my-server)


;; Configure use-package:
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)


(use-package python
  :ensure t
  :config
  (unbind-key "C-c <" python-mode-map)
  (unbind-key "C-c >" python-mode-map)
  :bind
  (:map python-mode-map
        ("M-7" . python-shell-switch-to-shell)
        ("C-," . python-indent-shift-left)
        ("C-." . python-indent-shift-right)
        ("M-U" . beginning-of-defun)
        ("M-O" . end-of-defun)
        )
  :bind
  (:map inferior-python-mode-map
        ("M-r" . kill-word)
        ("C-r" . comint-history-isearch-backward-regexp)
        ))


(use-package sgml-mode
  :ensure t
  :bind
  ("M-7" . sgml-tag)
  ("M-O" . sgml-skip-tag-forward)
  ("M-U" . sgml-skip-tag-backward)
  )


(use-package magit
  :ensure t
  :config
  (unbind-key "M-1" magit-mode-map)
  (unbind-key "M-2" magit-mode-map)
  (unbind-key "M-3" magit-mode-map)
  (unbind-key "M-4" magit-mode-map)
  (unbind-key "M-w" magit-mode-map)
  (unbind-key "C-w" magit-mode-map)
  :bind
  ("<f8>" . magit-status)
  ("C-x g" . magit-status)
  ("C-x C-g" . magit-dispatch-popup)
  (:map magit-mode-map
	       ("1" . magit-section-show-level-1-all)
	       ("2" . magit-section-show-level-2-all)
	       ("3" . magit-section-show-level-3-all)
	       ("4" . magit-section-show-level-4-all)
	       ("M-c" . magit-copy-buffer-revision)
	       ("M-x" . magit-copy-section-value)
	       ))


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
  :bind (:map js2-mode-map
	      ("M-7" . nodejs-repl)
	      ("M-j" . backward-char)
	      ("C-n" . js2-next-error)
	      ))
(defun my-js2-mode-hook ()
  (electric-pair-mode 1))


(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode)
  :init
  (add-hook 'json-mode-hook 'my-json-mode-hook))
(defun my-json-mode-hook ()
  (electric-pair-mode 1))


(use-package json-reformat
  :ensure t)


(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'my-rust-mode-hook))
(defun my-rust-mode-hook ()
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
