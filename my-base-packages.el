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
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))


(use-package diminish
  :ensure t
  :demand t
  :config
  (diminish 'auto-revert-mode)
  (diminish 'subword-mode)
  (diminish 'which-key-mode)
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode))


(use-package ido
  :demand t

  :init
  ;; Install flx-ido, which improves ido's flex matchig.
  (use-package flx-ido
    :ensure t
    :commands (flx-ido-mode))

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


(use-package project
  :ensure t
  :demand t
  :custom (project-switch-commands 'project-dired)
  :config (define-key my-commands-keymap (kbd "p") project-prefix-map)
  :bind (("M-2" . project-switch-project)
         :map my-commands-keymap
         ("d" . project-dired)
         ("s" . project-shell)
         ("y" . project-find-regexp)
         :map project-prefix-map
         ("C-b" . nil)
         ("l" . project-list-buffers)
         ("w" . project-forget-project)))


(use-package company
  :ensure t
  :demand t
  :diminish company-mode
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


(use-package envrc
  :ensure t
  :demand t
  :bind (:map my-commands-keymap
         ("u e" . envrc-command-map)))


(use-package flymake
  :commands (flymake-mode)
  :bind (:map my-commands-keymap
         ("f" . flymake-show-buffer-diagnostics)
         ("F" . flymake-show-project-diagnostics)
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


(use-package yasnippet
  :ensure t
  :commands (yas-global-mode yas-minor-mode yas-reload-all)
  :custom
  (yas-visit-from-menu t)
  (yas-prompt-functions
   '(my-yas-popup-isearch-prompt  ;; Use `popup` if available.
     yas-maybe-ido-prompt
     yas-dropdown-prompt
     yas-completing-prompt
     yas-no-prompt))
  :config
  ;; NOTE: The package `yasnippet-snippets` on MELPA is a good library of
  ;; snippets for many languages. The problem is that, once installed, it
  ;; automatically loads all the snippets, which takes quite some time. To
  ;; avoid this, we do not install the library, but pick up from it only the
  ;; snippets we may actually use, and copy them to ~/src/emacs/snippets.
  (add-to-list 'yas-snippet-dirs "~/src/emacs/snippets")
  (yas-reload-all)
  :bind (:map yas-minor-mode-map
         ;; Make M-<return> also expand snippets (in addition to <tab>).
         ("M-RET" . yas-expand)
         ("M-<return>" . yas-expand)
         :map yas-keymap
         ("M-f" . yas-skip-and-clear-or-delete-char)
         ;; Allow recursive snippet expansion with M-<return>.
         ("M-RET" . yas-expand)
         ("M-<return>" . yas-expand))
  :hook
  ;; Yasnippet can be used as a global mode, by executing the command
  ;; `(yas-global-mode 1)`. However, that causes all installed snippets to be
  ;; loaded eagerly, which takes some time. Instead, we delay the loading of
  ;; the snippets for the time when the first buffer that uses Yasnippet is
  ;; created.
  (emacs-lisp-mode . my-yas-minor-mode-except-for-scratch)
  (snippet-mode . yas-minor-mode)
  (sh-mode . yas-minor-mode)
  (html-mode . yas-minor-mode)
  (css-mode . yas-minor-mode)
  (sql-mode . yas-minor-mode)
  (python-mode . yas-minor-mode)
  (rst-mode . yas-minor-mode)
  (svelte-mode . yas-minor-mode)
  (js-mode . yas-minor-mode)
  (typescript-mode . yas-minor-mode))


(use-package popup
  :ensure t
  :commands (popup-menu*)
  :bind (:map popup-menu-keymap
         ;; Add many alternative ways to navigate the list in the popup.
         ("M-n" . popup-next)
         ("M-p" . popup-previous)
         ("M-k" . popup-next)
         ("M-i" . popup-previous)
         ("M-l" . popup-next)
         ("M-j" . popup-previous)
         ("M-y" . popup-next)
         ("M-Y" . popup-previous)
         ("TAB" . popup-next)
         ("<tab>" . popup-next)
         ("<backtab>" . popup-previous)))


(use-package eglot
  :ensure t
  :commands (eglot-ensure)
  :custom (eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs '(svelte-mode "svelteserver" "--stdio"))
  :bind (:map my-commands-keymap
         ("e r" . eglot-rename)
         ("e a" . eglot-code-actions))
  :hook
  (svelte-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (typescript-mode . eglot-ensure))

;; NOTE:
;;
;; * Run 'pip install "python-lsp-server[all]"' to install the pylsp server for
;;   Python.
;;
;; * Run 'npm install typescript-language-server typescript' to install the
;;   tsserver server for Typescript.
;;
;; * Run 'npm install svelte-language-server' to install the svelteserver
;;   server for Svelte.


(provide 'my-base-packages)

;;; my-base-packages.el ends here
