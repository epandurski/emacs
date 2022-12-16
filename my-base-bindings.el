;;; my-base-bindings.el --- Re-define the basic key bindings.

;;; Commentary:

;;; Code:

(require 'my-util-funcs)
(require 'ido)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Throw away the awful Emacs defaults, and define new ;;
;; "Ergoemacs"-like key-bindings.                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bind the original M-s to M-6. Use M-s to move the cursor to other
;; window.
(global-set-key (kbd "M-6") (lookup-key (current-global-map) (kbd "M-s")))
(global-set-key (kbd "M-s") 'other-window)

;; Bind the original M-g to M-'. Use M-g to kill everything from the
;; point to the end of the current line.
(global-set-key (kbd "M-'") (lookup-key (current-global-map) (kbd "M-g")))
(global-set-key (kbd "M-' M-'") 'goto-line) ;; easier than "M-' M-g"
(global-set-key (kbd "M-g") 'kill-line)

;; Use M-a (instead of M-x) to execute commands.
(global-set-key (kbd "M-a") 'execute-extended-command)

;; Moving the cursor. Use the "i", "k", "j", "l" and their neighbors
;; to move around.
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-k") 'next-line)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-u") 'backward-word)
(global-set-key (kbd "M-o") 'forward-word)
(global-set-key (kbd "M-K") 'scroll-up-command)
(global-set-key (kbd "M-J") 'back-to-indentation)
(global-set-key (kbd "M-L") 'move-end-of-line)
(global-set-key (kbd "M-I") 'scroll-down-command)
(global-set-key (kbd "M-U") 'backward-paragraph)
(global-set-key (kbd "M-O") 'forward-paragraph)
(global-set-key (kbd "M-h") 'move-beginning-of-line)
(global-set-key (kbd "M-,") 'my-toggle-end-beginning-of-buffer)
(global-set-key (kbd "M->") 'xref-pop-marker-stack) ;; jump back after "M-."
(global-set-key (kbd "C-p") 'previous-error)
(global-set-key (kbd "C-n") 'next-error)

;; Deleting (killing) text:
(global-set-key (kbd "M-f") 'delete-char)
(global-set-key (kbd "M-d") 'delete-backward-char)
(global-set-key (kbd "M-r") 'kill-word)
(global-set-key (kbd "M-e") 'backward-kill-word)

;; Selecting (marking) text:
(global-set-key (kbd "M-8") 'my-mark-current-symbol)
(global-set-key (kbd "M-*") 'mark-paragraph)
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; Manipulate the mark, copy, cut, paste, undo:
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "C-SPC") 'my-set-mark-command-with-prefix)
(global-set-key (kbd "C-@") 'my-set-mark-command-with-prefix)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-x") 'kill-region)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-V") 'yank-pop)
(global-set-key (kbd "M-z") 'undo)

;; Common text-editing actions:
(global-set-key (kbd "M-b") 'toggle-input-method)
(global-set-key (kbd "M-1") 'kmacro-end-and-call-macro)
(global-set-key (kbd "M-\\") 'cycle-spacing)
(global-set-key (kbd "M-w") 'my-open-previous-line)
(global-set-key (kbd "M-t") 'my-toggle-letter-case)
(global-set-key (kbd "M-T") 'my-upcase-letter-case)
(global-set-key (kbd "M-=") 'abbrev-prefix-mark)
(global-set-key (kbd "M-<return>") 'indent-new-comment-line) ;; also "C-M-j"
(global-set-key (kbd "C-k") 'zap-to-char)

;; Text search/replace:
(global-set-key (kbd "M-y") 'isearch-forward)
(global-set-key (kbd "M-Y") 'isearch-backward)
(global-set-key (kbd "M-5") 'query-replace)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "M-6") search-map)
(global-set-key (kbd "C-y") 'rgrep)

;; Manipulate buffers:
(global-set-key (kbd "M-m") 'ido-switch-buffer)
(global-set-key (kbd "M-M") 'list-buffers)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-s") 'my-save-buffer)
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-r") 'revert-buffer)
(global-set-key (kbd "<f5>") 'server-edit)

;; Manipulate windows:
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-4") 'delete-other-windows)
(global-set-key (kbd "<C-down>") 'shrink-window)
(global-set-key (kbd "<C-up>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; Unbind unused keys:
(dolist (key '("C-x 1" "C-x 2" "C-x 3" "C-x u" "C-x C-b"
               ;; These are especially convinient. Every major-mode
               ;; can define these as fast shortcuts.
               "C-d" "C-e" "C-f" "C-v" "C-b" "C-_" "C-\\"
               "M-0" "M-7" "M-0" "M-{" "M-}"))
  (global-unset-key (kbd key)))

;; Use "C-z" as "redo" when not on text terminal. Hide the menu bar on
;; text terminal.
(if window-system
    (global-set-key (kbd "C-z") 'undo)
  (menu-bar-mode -1))


;;;;;;;;;;;;;;;;;;;;;;;
;; Command shortcuts ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; bookmarks
(global-set-key (kbd "C-c b SPC") 'bookmark-set)
(global-set-key (kbd "C-c b b") 'my-bookmark-bmenu-list)

;; dired
(global-set-key (kbd "C-c d d") 'my-dired-at-home)
(global-set-key (kbd "C-c d j") 'dired-jump)
(global-set-key (kbd "C-c d SPC") 'ido-dired)
(global-set-key (kbd "C-c d p") 'dired-at-point)

;; shell
(global-set-key (kbd "C-c s s") 'shell)
(global-set-key (kbd "C-c s SPC") 'my-new-shell)
(global-set-key (kbd "C-c s c") 'shell-command)
(global-set-key (kbd "C-c s r") 'shell-command-on-region)
(global-set-key (kbd "C-c s e") 'eshell)

;; utilities
(global-set-key (kbd "C-c u p") 'proced)
(global-set-key (kbd "C-c u c") 'calendar)


(defun my-minibuffer-keys ()
  "My keybindings for the minibuffer."
  ;; Minibuffer uses "M-s" and "M-r" to search in history. Make them
  ;; do what they are supposed to do, and use "C-s" and "C-r" to
  ;; search in history instead.
  (define-key minibuffer-local-map (kbd "M-s") 'other-window)
  (define-key minibuffer-local-map (kbd "M-r") 'kill-word)
  (define-key minibuffer-local-map (kbd "C-s") 'next-matching-history-element)
  (define-key minibuffer-local-map (kbd "C-r") 'previous-matching-history-element)
  ;; Minibuffer uses "M-v" to switch to the completion buffer. Make it
  ;; do what it is supposed to do instead.
  (define-key minibuffer-local-completion-map (kbd "M-v") 'yank)
  (define-key minibuffer-local-completion-map (kbd "M-V") 'yank-pop)
  )
(eval-after-load "minibuffer" '(my-minibuffer-keys))

(defun my-isearch-mode-keys ()
  "My keybindings for `isearch' mode."
  ;; By default `buffer-menu-mode` uses "M-s" as a key prefix. Make it
  ;; do what it is supposed to do, and use "M-i" or "M-6" as a key
  ;; prefix instead.
  (define-key isearch-mode-map (kbd "M-i") (lookup-key isearch-mode-map (kbd "M-s")))
  (define-key isearch-mode-map (kbd "M-6") (lookup-key isearch-mode-map (kbd "M-s")))
  (define-key isearch-mode-map (kbd "M-s") 'other-window)
  ;; Add conveniences.
  (define-key isearch-mode-map (kbd "C-s") nil)
  (define-key isearch-mode-map (kbd "C-r") nil)
  (define-key isearch-mode-map (kbd "C-w") nil)
  (define-key isearch-mode-map (kbd "C-y") nil)
  (define-key isearch-mode-map (kbd "C-\\") nil)
  (define-key isearch-mode-map (kbd "M-k") 'isearch-yank-until-char)
  (define-key isearch-mode-map (kbd "M-d") 'isearch-del-char)
  (define-key isearch-mode-map (kbd "M-g") 'isearch-yank-line)
  (define-key isearch-mode-map (kbd "M-t") 'isearch-toggle-case-fold)
  (define-key isearch-mode-map (kbd "M-c") 'isearch-yank-char)
  (define-key isearch-mode-map (kbd "M-w") 'isearch-yank-word-or-char)
  (define-key isearch-mode-map (kbd "M-/") 'isearch-complete)
  (define-key isearch-mode-map (kbd "M-y") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-Y") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "M-5") 'isearch-query-replace)
  (define-key isearch-mode-map (kbd "M-%") 'isearch-query-replace-regexp)
  (define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
  (define-key isearch-mode-map (kbd "M-V") 'isearch-yank-pop)
  (define-key isearch-mode-map (kbd "M-b") 'isearch-toggle-input-method)
  )
(eval-after-load "isearch" '(my-isearch-mode-keys))

(defun my-ido-mode-keys ()
  "My keybindings for ido."
  ;; Use "M-y", "M-Y", "M-j", "M-l" for switching between the matching items.
  (define-key ido-common-completion-map (kbd "M-y") 'ido-next-match)
  (define-key ido-common-completion-map (kbd "M-Y") 'ido-prev-match)
  (define-key ido-common-completion-map (kbd "M-l") 'ido-next-match)
  (define-key ido-common-completion-map (kbd "M-j") 'ido-prev-match)
  ;; Change the original "M-s" binding for the file manager to "M-6",
  ;; and make sure "M-s" does what it is supposed to do.
  (define-key ido-file-completion-map (kbd "M-6") (lookup-key ido-file-completion-map (kbd "M-s")))
  (define-key ido-file-completion-map (kbd "M-s") 'other-window)
  ;; When finding files, `ido` overrides many of the navigation keys.
  ;; For example, "C-e" enters edit mode, "C-k" deletes the current
  ;; file, "M-m" creates a new directory. Here we bring back those
  ;; bindings that we really need.
  (define-key ido-file-completion-map (kbd "M-d") 'delete-backward-char)
  (define-key ido-file-completion-map (kbd "M-e") 'backward-kill-word)
  (define-key ido-file-completion-map (kbd "M-l") 'ido-next-match)
  (define-key ido-file-completion-map (kbd "M-j") 'ido-prev-match)
  (define-key ido-file-completion-map (kbd "M-i") 'ido-prev-work-directory)
  (define-key ido-file-completion-map (kbd "M-k") 'ido-next-work-directory)
  (define-key ido-file-completion-map (kbd "M-b") 'toggle-input-method)
  (define-key ido-file-completion-map (kbd "M-v") 'yank)
  (define-key ido-file-completion-map (kbd "M-w") 'ido-forget-work-directory)
  (define-key ido-file-completion-map (kbd "M-f") 'ido-wide-find-file-or-pop-dir)
  (define-key ido-file-completion-map (kbd "M-r") 'ido-wide-find-dir-or-delete-dir)
  (define-key ido-file-completion-map (kbd "M-y") 'ido-merge-work-directories)
  (define-key ido-file-completion-map (kbd "M-Y") 'ido-merge-work-directories)
  (define-key ido-file-completion-map (kbd "C-y") 'ido-merge-work-directories)
  ;; Use "C-o" to enter `ido-find-file` mode from `ido-switch-buffer`
  ;; mode ("C-f" does this too). Press "C-o" again to fallback to the
  ;; classic `find-file` mode.
  (define-key ido-buffer-completion-map (kbd "C-o") 'ido-enter-find-file)
  (define-key ido-file-completion-map (kbd "C-o") 'ido-fallback-command)
  ;; Press "M-m" again to fallback to the classic `switch-to-buffer`.
  (define-key ido-buffer-completion-map (kbd "M-m") 'ido-fallback-command)
  )
(eval-after-load "ido" '(my-ido-mode-keys))

(defun my-Buffer-menu-mode-keys ()
  "My keybindings for `Buffer-menu' mode."
  ;; `Buffer-menu-mode` uses "M-s" as a key prefix. Make it do what it
  ;; is supposed to do, and use "M-6" as a key prefix instead.
  (define-key Buffer-menu-mode-map (kbd "M-6") (lookup-key Buffer-menu-mode-map (kbd "M-s")))
  (define-key Buffer-menu-mode-map (kbd "M-s") 'other-window)
  ;; By default, "C-o" in Buffer-menu-mode displays a file in another
  ;; window -- make it run "find-file" instead, and use "M-f" for
  ;; displaying a file in another window.
  (define-key Buffer-menu-mode-map (kbd "C-o") 'find-file)
  (define-key Buffer-menu-mode-map (kbd "M-f") 'Buffer-menu-switch-other-window)
  )
(eval-after-load "buff-menu" '(my-Buffer-menu-mode-keys))

(defun my-bookmark-bmenu-mode-keys ()
  "My keybindings for bookmark-bmenu-mode."
  ;; By default, "C-o" in bookmark-mode displays a file in another
  ;; window -- make it run "find-file" instead, and use "M-f" for
  ;; displaying a file in another window.
  (define-key bookmark-bmenu-mode-map (kbd "C-o") 'find-file)
  (define-key bookmark-bmenu-mode-map (kbd "M-f") 'bookmark-bmenu-switch-other-window)
  )
(eval-after-load "bookmark" '(my-bookmark-bmenu-mode-keys))

(defun my-dired-mode-keys ()
  "My keybindings for `dired' mode."
  (define-key dired-mode-map (kbd "a") 'dired-omit-mode)
  (define-key dired-mode-map (kbd "M-5") 'find-name-dired)
  (define-key dired-mode-map (kbd "M-%") 'find-dired)
  ;; Make so that when the left button is clicked (dired translates
  ;; the left to <mouse-2>) directories are open in-place, and files
  ;; are open in other window.
  (define-key dired-mode-map (kbd "<mouse-2>") 'my-dired-mouse-find-file)
  ;; By default, "C-o" in dired-mode displays a file in another window
  ;; -- make it run "find-file" instead, and use "M-f" for displaying
  ;; a file in another window.
  (define-key dired-mode-map (kbd "C-o") 'find-file)
  (define-key dired-mode-map (kbd "M-f") 'my-dired-display-file)
  ;; `dired-mode` uses "M-s" as a key prefix. Make it do what it is
  ;; supposed to do, and use "M-6" as a key prefix instead.
  (define-key dired-mode-map (kbd "M-6") (lookup-key dired-mode-map (kbd "M-s")))
  (define-key dired-mode-map (kbd "M-s") 'other-window)
  )
(eval-after-load "dired" '(my-dired-mode-keys))

(defun my-shell-mode-keys ()
  "My keybindings for shell-mode."
  ;; By default, "M-r" in shell-mode searches in history -- restore it
  ;; to "kill-word", and use "C-r" for searching in history.
  (define-key shell-mode-map (kbd "M-r") 'kill-word)
  (define-key shell-mode-map (kbd "C-r") 'comint-history-isearch-backward-regexp)
  )
(eval-after-load "shell" '(my-shell-mode-keys))

(provide 'my-base-bindings)

;;; my-base-bindings.el ends here
