(in-package #:todo-project)

(defun client-app-settings ()
  (ps

    (defvar *app-settings* (create hide-done-items false))

    (defun update-app-settings ()
      (let ((input-hide-done-items (@ (chain document (get-element-by-id "hide-done")) checked)))
        (setf (@ *app-settings* hide-done-items) input-hide-done-items)
        (send-to-server "/setting-data" "PUT" *app-settings*)
        (render-todo-list todo-list)))))
