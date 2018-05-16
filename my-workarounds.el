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


(provide 'my-workarounds)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; my-workarounds.el ends here
