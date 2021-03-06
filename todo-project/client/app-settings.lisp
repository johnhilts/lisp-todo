(in-package #:todo-project)

(defun client-app-settings ()
  "define functions related to app settings"
  (ps
    (defvar *app-settings* (create hide-done-items false filter-text ""))))

(define-for-ps get-app-settings-from-server ()
  "define callback and make call to get app settings from server, then re-render html elements"
  (flet ((call-back ()
           (let ((server-app-settings (chain -j-s-o-n (parse (@ this response-text)))))
             (setf *app-settings* server-app-settings)
             (render-app-settings)
             (render-todo-filter)
             t)))
    (get-from-server *app-settings-api-endpoint* call-back)))

(define-for-ps update-app-settings ()
  "update app settings on client and server and re-render html elements"
  (let ((input-hide-done-items (@ (chain document (get-element-by-id "hide-done")) checked))
        (input-filter-text (@ (chain document (get-element-by-id "todo-filter-text")) value)))
    (setf (@ *app-settings* hide-done-items) input-hide-done-items)
    (setf (@ *app-settings* filter-text) input-filter-text)
    (send-to-server *app-settings-api-endpoint* "PUT" *app-settings*)
    (render-todo-list todo-list)))
