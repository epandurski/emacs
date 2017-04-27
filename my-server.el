;;; my-server.el --- Start an emacs server that smartly handles
;;; `delete-frame' event.

;;; Commentary:

;;; Code:

(defun my-handle-delete-frame (event)
  "Handle `delete-frame' events from the X server.

Closes the frame if it is not the last frame.  Otherwise kills
the current buffer and minimizes the only frame.  EVENT is a
`delete-frame' event."
  (interactive "e")
  (if (cadr (frame-list))
      (handle-delete-frame event)
    ;; there is only one frame
    (let ((window (selected-window)))
      (unless (window-minibuffer-p window)
	(kill-buffer (window-buffer window)))
      (iconify-frame)
      (delete-frame))))

(require 'server)
(unless (server-running-p)
  (define-key special-event-map [delete-frame] 'my-handle-delete-frame)
  (server-start))

(provide 'my-server)

;;; my-server.el ends here
