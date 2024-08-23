;;; my-project-customizations.el --- Customizations for project.el

;;; Commentary:

;;; Code:

(require 'project)

;; Go projects have "go.mod" files in their root directories. However,
;; project.el does not know about this. Here we teach it to recognize
;; "go.mod" files as project roots.
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)


(provide 'my-project-customizations)

;;; my-project-customizations.el ends here
