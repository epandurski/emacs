;; -*-no-byte-compile: t; -*-

;;; Commentary:
;;;
;;; This is my main Emacs configuration file.

;;; Code:

(defvar my-emacs-load-start (current-time))

;; Set and load the custom configurations file.
(setq custom-file "~/src/emacs/custom.el")
(load custom-file 'noerror nil t)

(if (display-graphic-p)
    (custom-set-faces
     ;; font
     '(default ((t (
       :family "DejaVu Sans Mono"
       :foundry "unknown"
       :slant normal
       :weight normal
       :height 128
       :width normal))))
     ;; current line
     '(hl-line ((t (
       :inherit highlight
       :extend t
       :background "gray93"))))
     ;; current line's number
     '(line-number-current-line ((t (
       :inherit line-number
       :background "gray84"))))
     ;; mode line
     '(mode-line ((((class color) (min-colors 88)) (
        :background "#9dbde4"
        :foreground "black"
        :box (:line-width -1 :style released-button)))))
     ;; selected region
     '(region ((t (
        :background "LightGoldenrod2")))))
  ;; Hide the menu bar on text terminal.
  (menu-bar-mode -1))

(defalias 'yes-or-no-p 'y-or-n-p)
(require 'uniquify)
(recentf-mode t)
(savehist-mode t)
(electric-pair-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(minibuffer-depth-indicate-mode)
(setq-default dired-omit-mode t)
(setq-default abbrev-mode t)
(setq read-process-output-max (* 256 1024)) ; 256kb
(setq nobreak-char-ascii-display t)
(setq default-frame-alist '((width . 80) (height . 0.8)))
(add-to-list 'load-path "~/src/emacs")
(byte-recompile-directory "~/src/emacs" 0)

;; Show line number for programs.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Use flyspell-mode
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Use dired+
(add-hook 'dired-load-hook (lambda ()
   (load "dired-x")))

;; Disable abberv-mode in the command shell.
(add-hook 'comint-mode-hook (lambda ()
   (abbrev-mode -1)))

;; Save abbrev usage statistics before exiting Emacs.
(add-hook 'kill-emacs-hook 'write-abbrev-file)

;; This instructs Emacs to use the OSC information, if present in the shell
;; prompt, to track the actual directory. For how to include OCS information in
;; the shell prompt, see the documentation for `comint-osc-directory-tracker`.
(add-hook 'comint-output-filter-functions 'comint-osc-process-output)

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

;; If necessary, upgrade flymake's version. Emacs 28 comes with an
;; improved version of flymake, which we would want to use even in
;; older Emacs.
(require 'my-util-funcs)
(my-upgrade-builtin-package 'flymake '(1 2 2))

;; A new flymake version, may install as a dependency, a new eldoc
;; version. It seems though, that eldoc is loaded automatically very
;; early (before `package-initialize`), and therefore we may need to
;; re-load the new eldoc version here.
(if (assq 'eldoc package-alist)
    ;; I guess we do not need (unload-feature 'eldoc) here.
    (load "eldoc"))

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
(require 'my-workarounds)
(require 'my-server)
(require 'my-base-bindings)
(require 'my-base-packages)
(require 'my-lang-packages)

;; Abbrev-expand only in non-prog-modes, or in strings and comments.
(setq abbrev-expand-function 'my-abbrev-expand-function)

;; `elpa-mirror` is a module that creates a local Emacs package
;; repository from installed packages, so that package upgrade never
;; breaks (see https://github.com/redguardtoo/elpa-mirror). Use `M-x
;; elpamr-create-mirror-for-installed` to (re)create the local
;; repository.
(autoload 'elpamr-create-mirror-for-installed "elpa-mirror"
  "Export installed packages into a new directory." t)

;; `envrc` uses the direnv tool to determine per-directory/project environment
;; variables and then set those environment variables on a per-buffer basis.
;; This requires direnv to be installed.
(envrc-global-mode)

;; Calculate and show the time it took to execute this file.
(let ((t0 (car (time-convert my-emacs-load-start 1000)))
      (t1 (car (time-convert (current-time) 1000))))
  (message "init.el loaded in %dms" (- t1 t0)))

(provide 'my-init)

;;; init.el ends here
