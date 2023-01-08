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
 '(auto-save-default nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-scroll-output 'first-error)
 '(create-lockfiles nil)
 '(cua-remap-control-v nil)
 '(default-input-method "bulgarian-phonetic")
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
 '(fill-column 79)
 '(find-ls-option
   '("-exec ls -ldhb --time-style=long-iso --group-directories-first {} +" . "-ldhb --time-style=long-iso --group-directories-first"))
 '(fringe-mode '(nil . 0) nil (fringe))
 '(global-so-long-mode t)
 '(global-subword-mode t)
 '(help-at-pt-display-when-idle '(flymake-overlay) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.9)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(json-reformat:indent-width 2)
 '(make-backup-files nil)
 '(recentf-max-saved-items 100)
 '(ring-bell-function 'ignore)
 '(save-interprogram-paste-before-kill t)
 '(scroll-bar-mode 'right)
 '(search-whitespace-regexp "[ \t\r\n]+")
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(so-long-threshold 500)
 '(tool-bar-mode nil)
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 128 :width normal))))
 '(highlight ((t (:background "lavender"))))
 '(mode-line ((((class color) (min-colors 88)) (:background "#9dbde4" :foreground "black" :box (:line-width -1 :style released-button)))))
 '(region ((t (:background "LightGoldenrod2")))))


