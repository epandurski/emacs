;; -*-no-byte-compile: t; -*-

;;; Commentary:
;;;
;;; This is my main Emacs configuration file.

;;; Code:

(defvar my-emacs-load-start (current-time))

;; Set and load the custom configurations file.
(setq custom-file "~/src/emacs/custom.el")
(load custom-file 'noerror nil t)

;; Hide the menu bar on text terminal.
(unless window-system
  (menu-bar-mode -1))

(defalias 'yes-or-no-p 'y-or-n-p)
(require 'uniquify)
(recentf-mode t)
(electric-pair-mode 1)
(setq-default dired-omit-mode t)

;; Use dired+
(add-hook 'dired-load-hook (lambda ()
   (load "dired-x")
   ;; Set dired-x global variables.
   (customize-set-variable 'dired-guess-shell-alist-user '(
      ("\\.\\(mp3\\|ogg\\|wav\\)\\'" "audacious")
      ("\\.\\(mp4\\|webm\\|mov\\|avi\\)\\'" "vlc")))
   ))

;; Add code navigation commands to the Edit menu. This makes easier to
;; navigate code using only the mouse.
(require 'easymenu)
(easy-menu-add-item nil '("edit") ["--" nil t])
(easy-menu-add-item nil '("edit") ["Jump to Definition" xref-find-definitions t])
(easy-menu-add-item nil '("edit") ["Jump Back from Definition" xref-pop-marker-stack t])

;; Configure package archives. Comment/uncomment lines to
;; enable/disable archives as desired:
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

;; Circumvent a bug in Emacs 26.1 (fixed in 27.1).
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)

;; A list of packages that must be automatically installed, if they
;; are not installed already.
(setq my-package-list '(use-package))

;; Ensure all packages in `my-package-list` are installed.
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

(eval-when-compile
  (require 'use-package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run my configuration scripts ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/src/emacs")
(byte-recompile-directory "~/src/emacs" 0)

;; If necessary, upgrade versions of builtin packages.
(require 'my-util-funcs)
(my-upgrade-builtin-package 'flymake '(1 2 2))

(require 'my-workarounds)
(require 'my-server)
(require 'my-abbrevs)
(require 'my-base-bindings)
(require 'my-base-packages)
(require 'my-lang-packages)
(require 'my-legacy-packages)

;; `elpa-mirror` is a module that creates a local Emacs package
;; repository from installed packages, so that package upgrade never
;; breaks (see https://github.com/redguardtoo/elpa-mirror). Use `M-x
;; elpamr-create-mirror-for-installed` to (re)create the local
;; repository.
(require 'elpa-mirror)

;; Calculate and show the time it took to execute this file.
(let ((t0 (car (time-convert my-emacs-load-start 1000)))
      (t1 (car (time-convert (current-time) 1000))))
  (message "init.el loaded in %dms" (- t1 t0)))

(provide 'my-init)

;;; init.el ends here
