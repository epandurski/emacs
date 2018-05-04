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

(provide 'my-util-funcs)

;;; my-util-funcs.el ends here
