(in-package #:todo-project)

(defun client-app-settings ()
  "define functions related to app settings"
  (ps

    (defvar *app-settings* (create hide-done-items false))

    (defun get-app-settings-from-server ()
      "define callback and make call to get app settings from server, then re-render html elements"
      (flet ((call-back ()
               (let ((server-app-settings (chain -j-s-o-n (parse (@ this response-text)))))
                 (setf *app-settings* server-app-settings)
                 (render-app-settings)
                 t)))
        (get-from-server *app-settings-api-endpoint* call-back)))

    (defun update-app-settings ()
      "update app settings on client and server and re-render html elements"
      (let ((input-hide-done-items (@ (chain document (get-element-by-id "hide-done")) checked)))
        (setf (@ *app-settings* hide-done-items) input-hide-done-items)
        (send-to-server *app-settings-api-endpoint* "PUT" *app-settings*)
        (render-todo-list todo-list)))))
