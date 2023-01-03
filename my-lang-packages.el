;; -*-no-byte-compile: t; -*-

;;; my-lang-packages.el --- Ensure the packages supporting differnet
;;; language formats are installed and configured.

;;; Commentary:

;;; Code:

(require 'my-base-bindings)
(require 'my-util-funcs)
(eval-when-compile
  (require 'use-package))


(use-package python
  :commands (python-mode)
  :custom (python-shell-interpreter "python3")
  :config
  (add-hook 'python-mode-hook #'hs-minor-mode)
  (add-hook 'python-mode-hook #'imenu-add-menubar-index)
  (add-hook 'python-mode-hook #'flyspell-prog-mode)
  :bind (:map python-mode-map
         ("M-9" . python-indent-shift-left)
         ("M-0" . python-indent-shift-right)
         ("M-U" . beginning-of-defun)
         ("M-O" . end-of-defun)))


(use-package js
  :commands (js-mode)
  :custom (js-indent-level 2)
  :config
  (add-hook 'js-mode-hook #'hs-minor-mode)
  (add-hook 'js-mode-hook #'imenu-add-menubar-index)
  (add-hook 'js-mode-hook #'flyspell-prog-mode))


(use-package typescript-mode
  :ensure t
  :commands (typescript-mode)
  :custom (typescript-indent-level 2)
  :config
  (add-hook 'typescript-mode-hook #'hs-minor-mode)
  (add-hook 'typescript-mode-hook #'flyspell-prog-mode))


(use-package sgml-mode
  :commands (html-mode sgml-mode)
  :custom (sgml-validate-command "tidy --gnu-emacs yes -utf8 -e -q")
  :bind (:map html-mode-map
         ("M-7" . sgml-delete-tag)
         ("M-9" . sgml-tag)
         ("M-0" . sgml-close-tag)
         ("M-O" . sgml-skip-tag-forward)
         ("M-U" . sgml-skip-tag-backward)))


(use-package mhtml-mode
  ;; Mhtml-mode is a sub-mode of sgml-mode. By default, html/htm files
  ;; are open in this mode.
  :commands (mhtml-mode))


(use-package jinja2-mode
  :ensure t
  :commands (jinja2-mode)
  :config (add-hook 'jinja2-mode-hook #'flyspell-prog-mode))


(use-package svelte-mode
  :ensure t
  :commands (svelte-mode))


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :custom
  (markdown-enable-prefix-prompts nil)
  (markdown-asymmetric-header t)
  (markdown-xhtml-header-content
   (concat
    "<style type=\"text/css\">"
    (my-file-to-string "~/src/emacs/splendor.css")
    "</style>")
   "Add a decent style-sheet.")
  (markdown-command
   (concat
    "cmark-gfm"
    " -e table"
    " -e tasklist"
    " -e strikethrough"
    " -e tagfilter"
    " -e autolink"
    " -e footnotes"))
  :init
  (defvar my-markdown-mode-chordmap (make-keymap) "My chord-keymap for markdown-mode.")
  :config
  (key-chord-define markdown-mode-map "fd" my-markdown-mode-chordmap)
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  (add-hook 'markdown-mode-hook #'imenu-add-menubar-index)
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
         :map my-markdown-mode-chordmap
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


(use-package json-mode
  :ensure t
  :commands (json-mode)
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook #'flyspell-prog-mode)
  :bind (:map json-mode-map
         ("C-\\" . json-pretty-print-buffer)))


(use-package yaml-mode
  :ensure t
  :commands (yaml-mode)
  :mode "\\.yml\\'"
  :config (add-hook 'yaml-mode-hook #'flyspell-prog-mode))


(use-package dockerfile-mode
  :ensure t
  :commands (dockerfile-mode)
  :config (add-hook 'dockerfile-mode-hook #'flyspell-prog-mode))


(provide 'my-lang-packages)

;;; my-lang-packages.el ends here
