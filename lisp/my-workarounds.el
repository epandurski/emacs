;;; my-wokrarounds.el --- Workarounds for problems and bugs in emacs.

;;; Commentary:

;;; Code:


;; A workaround for a bug that has been fixed in Emacs 25.2
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))


;; A workaround for the fact that dired, when entering isearch mode,
;; sometimes tries to add a "M-s f f" binding in the 'isearch-mode-map
;; keymap. Because we have redefined "M-s" -- the whole thing
;; breaks. Here we redefine the offending functions in the 'dired-aux
;; module, substituting the hard-coded "M-s f f" with "M-6 f f" (which
;; points to the original "M-s" key prefix keymap).
(with-eval-after-load 'dired-aux
  (defun dired-isearch-filenames-setup ()
    "Set up isearch to search in Dired file names.  Intended to
be added to `isearch-mode-hook'."
    (when (or (eq dired-isearch-filenames t)
	      (and (eq dired-isearch-filenames 'dwim)
		   (get-text-property (point) 'dired-filename)))
      (define-key isearch-mode-map "\M-6ff" 'dired-isearch-filenames-mode) ;; Was "\M-sff" originally!
      (dired-isearch-filenames-mode 1)
      (add-hook 'isearch-mode-end-hook #'dired-isearch-filenames-end nil t))))

(with-eval-after-load 'dired-aux
  (defun dired-isearch-filenames-end ()
    "Clean up the Dired file name search after terminating
isearch."
    (define-key isearch-mode-map "\M-6ff" nil) ;; Was "\M-sff" originally!
    (dired-isearch-filenames-mode -1)
    (remove-hook 'isearch-mode-end-hook #'dired-isearch-filenames-end t)
    (unless isearch-suspended
      (kill-local-variable 'dired-isearch-filenames))))


(provide 'my-workarounds)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; my-workarounds.el ends here
