;;; my-base-bindings.el --- Re-define the basic key bindings.

;;; Commentary:

;;; Code:

(require 'my-util-funcs)

(defvar my-commands-keymap (make-keymap "Custom commands")
  "Custom commands invoked with a key-prefix.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Throw away the awful Emacs defaults, and define new ;;
;; "Ergoemacs"-like key-bindings.                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-;") my-commands-keymap)

;; Bind the original M-s to M-6. Use M-s to move the cursor to other
;; window.
(global-set-key (kbd "M-6") (lookup-key (current-global-map) (kbd "M-s")))
(global-set-key (kbd "M-s") 'other-window)

;; Bind the original M-g to M-=. Use M-g to kill everything from the
;; point to the end of the current line.
(global-set-key (kbd "M-=") (lookup-key (current-global-map) (kbd "M-g")))
(global-set-key (kbd "M-= M-g") nil)
(global-set-key (kbd "M-= g") nil)
(global-set-key (kbd "M-= M-=") 'goto-line) ;; much easier than "M-= M-g"
(global-set-key (kbd "M-= =") 'goto-line) ;; much easier than "M-= g"
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
(global-set-key (kbd "M-+") 'abbrev-prefix-mark)
(global-set-key (kbd "M-RET") 'indent-new-comment-line) ;; also "C-M-j"
(global-set-key (kbd "M-]") 'zap-to-char)
(global-set-key (kbd "M-H") 'flyspell-check-previous-highlighted-word)

;; Text search/replace:
(global-set-key (kbd "M-y") 'isearch-forward)
(global-set-key (kbd "M-Y") 'isearch-backward)
(global-set-key (kbd "M-5") 'query-replace)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "M-6") search-map)
(global-set-key (kbd "C-y") 'rgrep)

;; Manipulate buffers:
(global-set-key (kbd "M-m") 'switch-to-buffer)
(global-set-key (kbd "M-M") 'list-buffers)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-s") 'my-save-buffer)
(global-set-key (kbd "C-w") 'kill-current-buffer)
(global-set-key (kbd "C-r") 'revert-buffer)
(global-set-key (kbd "<f5>") 'server-edit)

;; Manipulate windows:
(global-set-key (kbd "M-2") 'my-arrange-windows)
(global-set-key (kbd "M-4") 'delete-other-windows)
(global-set-key (kbd "<C-down>") 'shrink-window)
(global-set-key (kbd "<C-up>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; Unbind unused keys:
(dolist (key '(
    ;; These are especially convinient. Every major-mode can define these as
    ;; fast shortcuts.
    "M-3" "M-7" "M-9" "M-0" "M--" "M-'"
    "M-_" "M-{" "M-}" "M-\""
    "C-e" "C-d" "C-f" "C-v" "C-b" "C-k" "C-\\"

    ;; NOTE: For some reason, "M-[" produces ESC on text terminals.

    ;; It seems that pressing "C-/", on text terminals generates "C-_".
    ;; Therefore, to work correctly on graphical and text terminals, both
    ;; should be bound to the same command.
    "C-/" "C-_"))
  (global-unset-key (kbd key)))

;; Use "C-z" as "redo" when not on text terminal.
(if (display-graphic-p)
    (global-set-key (kbd "C-z") 'undo))


;;;;;;;;;;;;;;;;;;;;;;;
;; Command shortcuts ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; To comment the selected region, "M-;" should be hit twice. "M-; ;" is an
;; alternative which does exactly the same.
(define-key my-commands-keymap (kbd "M-;") 'comment-dwim)
(define-key my-commands-keymap (kbd ";") 'comment-dwim)

;; dired
(define-key my-commands-keymap (kbd "d d") 'my-dired-at-home)
(define-key my-commands-keymap (kbd "d SPC") 'dired)
(define-key my-commands-keymap (kbd "d j") 'my-dired-jump)

;; shell
(define-key my-commands-keymap (kbd "s s") 'my-shell-at-home)
(define-key my-commands-keymap (kbd "s e") 'eshell)

;; bookmarks
(define-key my-commands-keymap (kbd "b b") 'bookmark-set)
(define-key my-commands-keymap (kbd "b l") 'my-bookmark-bmenu-list)
(define-key my-commands-keymap (kbd "M-b") 'my-bookmark-bmenu-list)

;; search
(define-key my-commands-keymap (kbd "y f") 'find-name-dired)
(define-key my-commands-keymap (kbd "y F") 'find-dired)
(define-key my-commands-keymap (kbd "y g") 'rgrep)

;; version control
(define-key my-commands-keymap (kbd "v p") 'project-vc-dir)

;; spell-checking
(define-key my-commands-keymap (kbd "l l") 'ispell)
(define-key my-commands-keymap (kbd "l w") 'ispell-word)
(define-key my-commands-keymap (kbd "l b") 'ispell-buffer)
(define-key my-commands-keymap (kbd "l r") 'ispell-region)
(define-key my-commands-keymap (kbd "l s") 'ispell-comments-and-strings)
(define-key my-commands-keymap (kbd "l c") 'ispell-continue)
(define-key my-commands-keymap (kbd "l k") 'ispell-kill-ispell)
(define-key my-commands-keymap (kbd "l d") 'ispell-change-dictionary)
(define-key my-commands-keymap (kbd "l f") 'flyspell-buffer)
(define-key my-commands-keymap (kbd "M-l") 'flyspell-buffer)

;; hs-minor-mode
(define-key my-commands-keymap (kbd "h h") 'hs-hide-all)
(define-key my-commands-keymap (kbd "h s") 'hs-show-all)
(define-key my-commands-keymap (kbd "h l") 'hs-hide-level)
(define-key my-commands-keymap (kbd "h t") 'hs-toggle-hiding)
(define-key my-commands-keymap (kbd "M-h") 'hs-toggle-hiding)

;; insert into current buffer
(define-key my-commands-keymap (kbd "i k") 'yank-from-kill-ring)
(define-key my-commands-keymap (kbd "i c") 'insert-char)
(define-key my-commands-keymap (kbd "i b") 'insert-buffer)
(define-key my-commands-keymap (kbd "i f") 'insert-file)
(define-key my-commands-keymap (kbd "i r") 'insert-register)
(define-key my-commands-keymap (kbd "M-i") 'yank-from-kill-ring)

;; open files and miscellaneous utilities
(define-key my-commands-keymap (kbd "o p") 'proced)
(define-key my-commands-keymap (kbd "o c") 'calendar)

;; file utilities
(define-key my-commands-keymap (kbd "f f") 'find-file-at-point)
(define-key my-commands-keymap (kbd "f j") 'my-dired-jump)
(define-key my-commands-keymap (kbd "M-f") 'my-dired-jump)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-specific bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "minibuffer" (lambda ()
  "My keybindings for the minibuffer."
  ;; Minibuffer uses "M-s" and "M-r" to search in history. Make them
  ;; do what they are supposed to do, and use "C-s" and "C-r" to
  ;; search in history instead.
  (define-key minibuffer-local-map (kbd "M-s") 'other-window)
  (define-key minibuffer-local-map (kbd "M-r") 'kill-word)
  (define-key minibuffer-local-map (kbd "C-s") 'next-matching-history-element)
  (define-key minibuffer-local-map (kbd "C-r") 'previous-matching-history-element)
  (define-key minibuffer-local-map (kbd "C-v") 'switch-to-completions)
  ;; Minibuffer uses "M-v" to switch to the completion buffer. Make it
  ;; do what it is supposed to do instead.
  (define-key minibuffer-local-completion-map (kbd "M-v") 'yank)
  (define-key minibuffer-local-completion-map (kbd "M-V") 'yank-pop)
  ))

(eval-after-load "isearch" (lambda ()
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
  (define-key isearch-mode-map (kbd "M-W") 'isearch-yank-char)
  (define-key isearch-mode-map (kbd "M-w") 'isearch-yank-word-or-char)
  (define-key isearch-mode-map (kbd "M-/") 'isearch-complete)
  (define-key isearch-mode-map (kbd "M-y") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-Y") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "M-5") 'isearch-query-replace)
  (define-key isearch-mode-map (kbd "M-%") 'isearch-query-replace-regexp)
  (define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
  (define-key isearch-mode-map (kbd "M-V") 'isearch-yank-pop)
  (define-key isearch-mode-map (kbd "M-b") 'isearch-toggle-input-method)
  ))

(eval-after-load "buff-menu" (lambda ()
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
  ))

(eval-after-load "bookmark" (lambda ()
  "My keybindings for bookmark-bmenu-mode."
  ;; By default, "C-o" in bookmark-mode displays a file in another
  ;; window -- make it run "find-file" instead, and use "M-f" for
  ;; displaying a file in another window.
  (define-key bookmark-bmenu-mode-map (kbd "C-o") 'find-file)
  (define-key bookmark-bmenu-mode-map (kbd "M-f") 'bookmark-bmenu-switch-other-window)
  ))

(eval-after-load "dired" (lambda ()
  "My keybindings for `dired' mode."
  (define-key dired-mode-map (kbd "a") 'dired-omit-mode)
  (define-key dired-mode-map (kbd "h") 'dired-hide-all)
  (define-key dired-mode-map (kbd "TAB") 'my-dired-toggle-subdir-visibility)
  (define-key dired-mode-map (kbd "M-p") 'dired-prev-subdir)
  (define-key dired-mode-map (kbd "M-n") 'dired-next-subdir)
  (define-key dired-mode-map (kbd "M-7") 'dired-tree-up)
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
  ))

(eval-after-load "comint" (lambda ()
  "My keybindings for shell-mode."
  ;; By default, "M-r" in shell-mode searches in history -- restore it
  ;; to "kill-word", and use "C-r" for searching in history.
  (define-key comint-mode-map (kbd "M-r") 'kill-word)
  (define-key comint-mode-map (kbd "C-r") 'comint-history-isearch-backward-regexp)
  ))

(eval-after-load "sgml-mode" (lambda ()
  "My keybindings for the html-mode."
  ;; Html-mode binds "M-o" to `facemenu-keymap', which is useless. Make it
  ;; do what it is supposed to do.
  (define-key html-mode-map (kbd "M-o") 'forward-word)
  ))

(provide 'my-base-bindings)

;;; my-base-bindings.el ends here
