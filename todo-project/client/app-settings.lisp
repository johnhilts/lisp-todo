(in-package #:todo-project)

(defun client-app-settings ()
  "define functions related to app settings"
  (ps:ps
    (defvar *app-settings* (ps:create hide-done-items false filter-text ""))))

(define-for-ps get-app-settings-from-server (&optional call-back)
  "define callback and make call to get app settings from server, then re-render html elements"
  (flet ((local-call-back ()
           (let ((server-app-settings (ps:chain -j-s-o-n (parse (@ this response-text)))))
             (setf *app-settings* server-app-settings)
             (unless (@ *app-settings* filter-text)
               (setf (@ *app-settings* filter-text) ""))
             (render-app-settings)
             (render-todo-filter)
             (call-back)
             t)))
    (get-from-server *app-settings-api-endpoint* local-call-back)))

(define-for-ps update-app-settings (&key (can-re-render t))
  "update app settings on client and server and re-render html elements"
  (let ((input-hide-done-items (@ (ps:chain document (get-element-by-id "hide-done")) checked))
        (input-filter-text (@ (ps:chain document (get-element-by-id "todo-filter-text")) value)))
    (setf (@ *app-settings* hide-done-items) input-hide-done-items)
    (setf (@ *app-settings* filter-text) input-filter-text)
    (setf (@ *app-settings* selected-filter-tag-todo-ids) *selected-filter-tag-todo-ids*)
    (setf (@ *app-settings* filter-tag-match-type) *filter-tag-match-type*)
    (send-to-server *app-settings-api-endpoint* "PUT" *app-settings*)
    (when can-re-render
        (render-todo-list (get-all-todos)))))
