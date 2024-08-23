;; -*-no-byte-compile: t; -*-

;;; Commentary:
;;;
;;; This is my main Emacs custom configurations file.

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-file-name "~/src/emacs/abbrev_defs")
 '(abbrev-suggest t)
 '(auto-save-default nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-scroll-output 'first-error)
 '(completions-detailed t)
 '(completions-format 'one-column)
 '(context-menu-mode t)
 '(copy-directory-create-symlink t)
 '(create-lockfiles nil)
 '(cua-remap-control-v nil)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("c8b83e7692e77f3e2e46c08177b673da6e41b307805cd1982da9e2ea2e90e6d7" "b02eae4d22362a941751f690032ea30c7c78d8ca8a1212fdae9eecad28a3587f" "b6269b0356ed8d9ed55b0dcea10b4e13227b89fd2af4452eee19ac88297b0f99" "e3a1b1fb50e3908e80514de38acbac74be2eb2777fc896e44b54ce44308e5330" "fb83a50c80de36f23aea5919e50e1bccd565ca5bb646af95729dc8c5f926cbf3" "24168c7e083ca0bbc87c68d3139ef39f072488703dcdd82343b8cab71c0f62a7" "9abe2b502db3ed511fea7ab84b62096ba15a3a71cdb106fd989afa179ff8ab8d" "3c821498a7f319c7cd3a8848f2511dc723a22be89c95bd5acdb78b8eb5180cc1" default))
 '(default-input-method "bulgarian-phonetic")
 '(describe-bindings-outline t)
 '(dired-dwim-target 'dired-dwim-target-next)
 '(dired-guess-shell-alist-user
   '(("\\.\\(mp3\\|ogg\\|wav\\)\\'" "audacious")
     ("\\.\\(mp4\\|webm\\|mov\\|avi\\)\\'" "vlc")))
 '(dired-isearch-filenames t)
 '(dired-listing-switches "-alh --time-style=long-iso --group-directories-first")
 '(dired-omit-files "^[#]\\|^[.]\\(?:[^.]\\|[.].+\\)")
 '(dired-recursive-copies 'top)
 '(dired-recursive-deletes 'top)
 '(dired-use-ls-dired t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(enable-recursive-minibuffers t)
 '(fill-column 70)
 '(find-ls-option
   '("-exec ls -ldhb --time-style=long-iso --group-directories-first {} +" . "-ldhb --time-style=long-iso --group-directories-first"))
 '(fringe-mode '(nil . 0) nil (fringe))
 '(global-auto-revert-non-file-buffers t)
 '(global-so-long-mode t)
 '(global-subword-mode t)
 '(help-at-pt-display-when-idle '(flymake-overlay) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(help-enable-symbol-autoload t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(isearch-wrap-pause t)
 '(ispell-dictionary "bg-w_english")
 '(json-reformat:indent-width 2)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(message-send-mail-function 'smtpmail-send-it)
 '(mml-secure-openpgp-sign-with-sender t)
 '(mode-line-compact t)
 '(mode-line-position-column-line-format '(" (L%l,%c)"))
 '(next-error-message-highlight t)
 '(only-global-abbrevs t)
 '(package-selected-packages
   '(go-mode cython-mode rust-mode dockerfile-mode yaml-mode json-mode markdown-mode svelte-mode jinja2-mode typescript-mode eglot yasnippet magit envrc company flx-ido monokai-pro-theme diminish which-key use-package))
 '(read-extended-command-predicate 'command-completion-default-include-p)
 '(read-minibuffer-restore-windows nil)
 '(recentf-max-saved-items 100)
 '(ring-bell-function 'ignore)
 '(save-abbrevs 'silently)
 '(save-interprogram-paste-before-kill t)
 '(scroll-bar-mode nil)
 '(search-whitespace-regexp "[ \t\r\n]+")
 '(sentence-end-double-space nil)
 '(size-indication-mode t)
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 465)
 '(smtpmail-stream-type 'ssl)
 '(tool-bar-mode nil)
 '(use-short-answers t)
 '(user-full-name "Evgeni Pandurski")
 '(user-mail-address "epandurski@gmail.com")
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 128 :width normal)))))
