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

(defun my-upcase-letter-case ()
  "Downcase the current word or text selection."
  (interactive)
  (let (p1 p2 (deactivate-mark nil))
    (if (region-active-p)
	(setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'symbol) ) )
	(setq p1 (car bds) p2 (cdr bds)) ) )
    (upcase-region p1 p2)))

(defun my-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.

Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
	(setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'symbol) ) )
	(setq p1 (car bds) p2 (cdr bds)) ) )
    (when (not (eq last-command this-command))
      (save-excursion
	(goto-char p1)
	(cond
	 ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
	 ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps"))
	 ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps"))
	 ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
	 ((looking-at "[[:upper:]]") (put this-command 'state "all caps"))
	 (t (put this-command 'state "all lower")))))
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")))))

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
functions to visit the file and directory, respectively.  If
omitted or nil, these arguments default to `find-file' and
`find-alternate-file', respectively.  See dired-mouse-find-file."
  (interactive "e")
  (or find-file-func (setq find-file-func 'find-file-other-window))
  (or find-dir-func (setq find-dir-func 'find-alternate-file))
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
        (find-alternate-file file)
      (display-buffer (find-file-noselect file) t))))

(defun my-new-shell ()
  "Start an inferior shell in a new buffer"
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
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

(provide 'my-util-funcs)

;;; my-util-funcs.el ends here
