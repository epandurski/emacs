;;; my-util-funcs.el --- Define useful utility functions.

;;; Commentary:

;;; Code:

(defun my-open-previous-line (arg)
  "Open a new line before the current one.

With ARG, insert ARG lines."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(defun my-toggle-end-beginning-of-buffer ()
  "Toggle between the end and the beginnning of buffer."
  (interactive)
  (cond
   ((eq (point) (point-max)) (goto-char (point-min)))
   ((eq (point) (point-min)) (goto-char (point-max)))
   (t (goto-char (point-max)))))

(defun my-dired-at-home ()
  "Open dired buffer at home directory."
  (interactive)
  (when (> (minibuffer-depth) 0)
    (abort-recursive-edit))
  (when (> (recursion-depth) 0)
    (exit-recursive-edit))
  (dired "~/"))

(defun my-dired-jump ()
  "Require dired-x and jump."
  (interactive)
  (require 'dired-x)
  (dired-jump))

(defun my-bookmark-bmenu-list ()
  "Open bookmarks list buffer."
  (interactive)
  (when (> (minibuffer-depth) 0)
    (abort-recursive-edit))
  (when (> (recursion-depth) 0)
    (exit-recursive-edit))
  (command-execute 'bookmark-bmenu-list))

(defun my--get-word-boundaries ()
  "Return the buffer location of the current word or text selection.
If there are no region selected, return the location of the
current word. Returns a zero-length interval, if a word can not
be found near the point. The returned value is a cons cell."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (or (bounds-of-thing-at-point 'word)
        (cons (point) (point)))))

(defun my-upcase-letter-case ()
  "Downcase the current word or text selection."
  (interactive)
  (let (p1 p2 (deactivate-mark nil))
    (pcase-setq `(,p1 . ,p2) (my--get-word-boundaries))
    (upcase-region p1 p2)))

(defun my-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.

Toggles between: 'all lower', 'Init Caps', 'ALL CAPS'."
  (interactive)
  (let (p1 p2 state (deactivate-mark nil) (case-fold-search nil))
    (if (eq last-command this-command)
        (progn
          (setq p1 (get this-command 'p1))
	  (setq p2 (get this-command 'p2))
          (setq state (get this-command 'state)))
      (pcase-setq `(,p1 . ,p2) (my--get-word-boundaries))
      (save-excursion
	(goto-char p1)
        (setq state (cond
	       ((looking-at "[^[:word:]]*[[:upper:]][[:upper:]]") "all caps")
	       ((looking-at "[^[:word:]]*[[:upper:]][[:lower:]]") "init caps")
	       ((looking-at "[^[:word:]]*[[:upper:]]") "all caps")
               ("all lower"))))
      (put this-command 'p1 p1)
      (put this-command 'p2 p2))
    (put this-command 'state (cond
          ((string= "all lower" state)
           (upcase-initials-region p1 p2) "init caps")
          ((string= "init caps" state)
           (upcase-region p1 p2) "all caps")
          ((string= "all caps" state)
           (downcase-region p1 p2) "all lower")))))

(defun my-mark-current-symbol (&optional arg allow-extend)
  "Put point at beginning of current symbol, set mark at end."
  (interactive "p\np")
  (setq arg (if arg arg 1))
  (if (and allow-extend
	   (or (and (eq last-command this-command) (mark t))
	       (region-active-p)))
      (set-mark
       (save-excursion
	 (when (< (mark) (point))
	   (setq arg (- arg)))
	 (goto-char (mark))
	 (forward-symbol arg)
	 (point)))
    (let ((wbounds (bounds-of-thing-at-point 'symbol)))
      (unless (consp wbounds)
	(error "No symbol at point"))
      (if (>= arg 0)
	  (goto-char (car wbounds))
	(goto-char (cdr wbounds)))
      (push-mark (save-excursion
		   (forward-symbol arg)
		   (point)))
      (activate-mark))))

(defun my-set-mark-command-with-prefix ()
  "Put point at beginning of current symbol, set mark at end."
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-interactively 'set-mark-command)))

(defun my-dired-mouse-find-file (event &optional find-file-func find-dir-func)
  "In Dired, visit the file or directory name you click on.
The optional arguments FIND-FILE-FUNC and FIND-DIR-FUNC specify
functions to visit the file and directory, respectively. If
omitted or nil, these arguments default to `find-file-other-window'
and `dired', respectively. See dired-mouse-find-file."
  (interactive "e")
  (or find-file-func (setq find-file-func 'find-file-other-window))
  (or find-dir-func (setq find-dir-func 'dired))
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos (posn-point (event-end event)))
      (if (not (windowp window))
          (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist)
                 (dired-goto-subdir file))
            (progn
              (select-window window)
              (funcall find-dir-func file)))
      (select-window window)
      (funcall find-file-func (file-name-sans-versions file t)))))

(defun my-dired-display-file ()
  "In Dired, display this file in another window. If it is a
directory, display it in the same window."
  (interactive)
  (let (file)
    (setq file (dired-get-file-for-visit))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist)
                 (dired-goto-subdir file))
            (dired file))
      (display-buffer (find-file-noselect file) t))))

(defun my-shell-at-home ()
  "Switch to an inferior shell at home directory."
  (interactive)
  (let ((default-directory "~"))
    (call-interactively 'shell)))

(defun my-save-buffer (arg)
  "Save the current buffer, ask for filename if prefixed with C-u."
  (interactive "p")
  (if (eq arg 4)
      (ido-write-file)
    (save-buffer)))

(defun my-file-to-string (file)
  "Read file into string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun my-dired-toggle-subdir-visibility (arg)
  "Hide/show subdir and move the point to the subdir."
  (interactive "p")
  (let ((dir (dired-current-directory)))
    (dired-hide-subdir arg)
    (dired-goto-subdir dir)))

(defun my-upgrade-builtin-package (pkg version)
  "Try to update the version of PKG to VERSION if necessary.
PKG must be a symbol identifying the package, and VERSION must be
a list with version numbers.  For example, '(1 2 3) for version
`1.2.3`."
  (require 'package)
  (let ((builtin (package-built-in-p pkg version))
        (installed (assq pkg package-alist)))
    (unless (or builtin installed)
      (unless package-archive-contents
        (package-refresh-contents))
      (message "Installing new version of %s..." pkg)
      (let ((pdescr (cadr(assq pkg package-archive-contents))))
        (package-install pdescr)))))

(defun my-find-dired (arg)
  "Find files in dired, asking the user for a wildcard.
If prefixed with `C-u`, instead of wildcard, ask the user to
directly enter parameters for the `find` command."
  (interactive "p")
  (if (eq arg 4)
      (call-interactively 'find-dired)
    (call-interactively 'find-name-dired)))

(defun my-arrange-two-windows (window1 window2)
  "A helper function for `my-arrange-windows`."
  (let ((left1 (car (window-edges window1)))
        (left2 (car (window-edges window2)))
        (buffer2 (window-buffer window2)))
    (delete-other-windows)
    (if (= left1 left2)
        (split-window-right)
      (split-window-below))
    (other-window 1)
    (switch-to-buffer buffer2 t t)
    (other-window -1)))

(defun my-arrange-windows ()
  "Toggle between 2 window vertical-split, and horizontal-split."
  (interactive)
  (let* ((windows (window-list nil 'nominibuf nil))
         (n (length windows)))
    (if (<= n 2)
        (cond
         ((= n 1) (split-window-right))
         ((= n 2) (my-arrange-two-windows (car windows) (cadr windows))))
      (delete-other-windows)
      (split-window-right))))

(defun my-yas-popup-isearch-prompt (prompt choices &optional display-fn)
  "If `popup` is installed, use it to show a choice popup for Yasnippet.
This code has been copied almost verbatim from
www.emacswiki.org."
  (when (functionp 'popup-menu*)
    (require 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
        (popup-make-item
         (or (and display-fn (funcall display-fn choice))
             choice)
         :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(defun my-yas-minor-mode-except-for-scratch ()
  "Activate yas-minor-mode, unless buffer's name is *scratch*."
  (unless (equal (buffer-name) "*scratch*")
    (yas-minor-mode)))

(defun my-set-yasnippet-fixed-indent ()
  "Declare yas-indent-line as buffer-local, and set it to 'fixed.
For example, this is needed for the correct expansion of Python's
Yasnippets."
  (set (make-local-variable 'yas-indent-line) 'fixed))

(defun my-magit-project-status ()
  "Run `magit-status' in the current project's root."
  (interactive)
  (magit-status-setup-buffer (project-root (project-current t))))

(defun my--point-at-string-or-comment ()
  "Figure out if the point is at string or comment."
  (let ((result (syntax-ppss)))
    (or (nth 3 result) (nth 4 result))))

(defvar my--orig-abbrev-expand-function
  (symbol-function abbrev-expand-function)
  "The original value of `abbrev-expand-function`.")

(defun my-abbrev-expand-function ()
  "Abbrev-expand only in non-prog-modes, or in strings and comments."
  (unless (and (derived-mode-p 'prog-mode)
               (not (my--point-at-string-or-comment)))
    (funcall my--orig-abbrev-expand-function)))

(provide 'my-util-funcs)

;;; my-util-funcs.el ends here
