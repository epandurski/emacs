;;; my-base-bindings.el --- Re-define the basic key bindings.

;;; Commentary:

;;; Code:

(require 'my-util-funcs)
(require 'ido)
(ido-mode 'buffer)
(setq ido-use-virtual-buffers t)

;; Configure ergoemacs-like global keys:
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "M-w") 'my-open-previous-line)
(global-set-key (kbd "M-s") 'other-window)
(global-set-key (kbd "M-a") 'execute-extended-command)
(global-set-key (kbd "M-m") 'ido-switch-buffer)
(global-set-key (kbd "M-M") 'list-buffers)
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
(global-set-key (kbd "M-f") 'delete-char)
(global-set-key (kbd "M-d") 'delete-backward-char)
(global-set-key (kbd "M-r") 'kill-word)
(global-set-key (kbd "M-e") 'backward-kill-word)
(global-set-key (kbd "M-g") 'kill-line)
(global-set-key (kbd "M-b") 'toggle-input-method)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-x") 'kill-region)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-V") 'yank-pop)
(global-set-key (kbd "M-y") 'isearch-forward)
(global-set-key (kbd "M-Y") 'isearch-backward)
(global-set-key (kbd "M-t") 'my-toggle-letter-case)
(global-set-key (kbd "M-T") 'my-upcase-letter-case)
(global-set-key (kbd "M-,") 'my-toggle-end-beginning-of-buffer)
(global-set-key (kbd "M-*") 'tags-loop-continue) ;; substitution for the original "M-,"
(global-set-key (kbd "M->") 'pop-tag-mark) ;; related to "M-." (find-tag)
(global-set-key (kbd "M-\\") 'cycle-spacing)
(global-set-key (kbd "M-1") 'kmacro-end-and-call-macro)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-4") 'delete-other-windows)
(global-set-key (kbd "M-5") 'query-replace)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "M-6") 'goto-line)
(global-unset-key (kbd "M-7")) ;; Every mode can use this as a fast shortcut.
(global-set-key (kbd "M-8") 'my-mark-current-symbol)
(global-set-key (kbd "M-9") 'my-dired-at-home)
(global-set-key (kbd "M-0") 'my-bookmark-bmenu-list)
(global-set-key (kbd "M-<return>") 'indent-new-comment-line)  ;; "C-M-j" does the same
(global-set-key (kbd "C-SPC") 'my-set-mark-command-with-prefix)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-y") 'rgrep)
(global-set-key (kbd "C-r") 'revert-buffer)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-d") 'bookmark-set)
(global-set-key (kbd "C-k") 'zap-to-char)
(global-set-key (kbd "C-p") 'previous-error)
(global-set-key (kbd "C-n") 'next-error)
(global-set-key (kbd "C-2") 'server-edit)
(global-set-key (kbd "<f7>") 'eshell)

;; Unbind unused keys:
(dolist (key '("C-x 1" "C-x 2" "C-x 3" "C-x u" "C-x C-b" "C-e" "C-f" "C-v" "C-b" "C-_"))
  (global-unset-key (kbd key)))

;; Use "C-z" when not on text terminal:
(when window-system
  (global-set-key (kbd "C-z") 'undo))

(defun my-dired-keys ()
  "My keybindings for dired."
  (define-key dired-mode-map (kbd "h") 'dired-omit-mode)
  )
(eval-after-load "dired" '(my-dired-keys))

(defun my-minibuffer-keys ()
  "My keybindings for the minibuffer."
  ;; By default "M-s" is used as a search prefix -- make it switch to
  ;; other window instead.
  (define-key minibuffer-local-map (kbd "M-s") 'other-window)
  ;; By default, "M-r" in minibuffer searches in history -- restore it
  ;; to "kill-word", and use "C-r" for searching in history.
  (define-key minibuffer-local-map (kbd "M-r") 'kill-word)
  (define-key minibuffer-local-map (kbd "C-r") 'previous-matching-history-element)
  ;; By default "M-v" in the minibuffer switches to the completion
  ;; buffer -- make it do yank instead.
  (define-key minibuffer-local-completion-map (kbd "M-v") 'yank)
  (define-key minibuffer-local-completion-map (kbd "M-V") 'yank-pop)
  )
(eval-after-load "minibuffer" '(my-minibuffer-keys))

(defun my-isearch-mode-keys ()
  "My keybindings for `isearch' mode."
  ;; By default Buffer-menu-mode uses "M-s" as a search prefix -- make
  ;; it switch to other window instead.
  (define-key isearch-mode-map (kbd "M-s") 'other-window)
  ;; Add some conveniences.
  (define-key isearch-mode-map (kbd "M-y") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-Y") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "M-5") 'isearch-query-replace)
  (define-key isearch-mode-map (kbd "M-%") 'isearch-query-replace-regexp)
  (define-key isearch-mode-map (kbd "M-v") 'isearch-yank-kill)
  (define-key isearch-mode-map (kbd "M-V") 'isearch-yank-pop)
  (define-key isearch-mode-map (kbd "C-y") 'isearch-occur)
  )
(eval-after-load "isearch" '(my-isearch-mode-keys))

(defun my-ido-keys ()
  "My keybindings for ido."
  ;; Use "M-y", "M-Y", "M-j", "M-l" for switching between the matching items.
  (define-key ido-common-completion-map (kbd "M-y") 'ido-next-match)
  (define-key ido-common-completion-map (kbd "M-Y") 'ido-prev-match)
  (define-key ido-common-completion-map (kbd "M-l") 'ido-next-match)
  (define-key ido-common-completion-map (kbd "M-j") 'ido-prev-match)
  ;; When finding files, ido uses "M-s" as a search prefix -- make it
  ;; switch to other window instead.
  (define-key ido-file-completion-map (kbd "M-s") 'other-window)
  ;; When finding files, `ido` overrides many of the navigation keys.
  ;; For example, "C-e" enters edit mode, "C-k" deletes the current
  ;; file, "M-m" creates a new directory. Here we bring the back those
  ;; bindings that we really need.
  (define-key ido-file-completion-map (kbd "M-f") 'delete-char)
  (define-key ido-file-completion-map (kbd "M-d") 'delete-backward-char)
  (define-key ido-file-completion-map (kbd "M-r") 'kill-word)
  (define-key ido-file-completion-map (kbd "M-e") 'backward-kill-word)
  (define-key ido-file-completion-map (kbd "M-l") 'ido-next-match)
  (define-key ido-file-completion-map (kbd "M-j") 'ido-prev-match)
  (define-key ido-file-completion-map (kbd "M-o") 'ido-next-work-file)
  (define-key ido-file-completion-map (kbd "M-u") 'ido-prev-work-file)
  (define-key ido-file-completion-map (kbd "M-b") 'toggle-input-method)
  (define-key ido-file-completion-map (kbd "M-v") 'yank)
  ;; Use "C-o" to enter `ido-find-file` mode from `ido-switch-buffer`
  ;; mode ("C-f" does this too). Press "C-o" again to fallback to the
  ;; classic `find-file` mode.
  (define-key ido-buffer-completion-map (kbd "C-o") 'ido-enter-find-file)
  (define-key ido-file-completion-map (kbd "C-o") 'ido-fallback-command)
  )
(eval-after-load "ido" '(my-ido-keys))

(defun my-Buffer-menu-mode-keys ()
  "My keybindings for `Buffer-menu' mode."
  ;; By default Buffer-menu-mode uses "M-s" as a search prefix -- make
  ;; it switch to other window instead.
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
  ;; By default, "C-o" in dired-mode displays a file in another window
  ;; -- make it run "find-file" instead, and use "M-f" for displaying
  ;; a file in another window.
  (define-key dired-mode-map (kbd "C-o") 'find-file)
  (define-key dired-mode-map (kbd "M-f") 'dired-display-file)
  ;; By default dired-mode uses "M-s" as a search prefix -- make it
  ;; switch to other window instead.
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
