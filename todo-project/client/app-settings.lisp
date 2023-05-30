(in-package #:todo-project)

(defun client-app-settings ()
  "define functions related to app settings"
  (ps:ps
    (defparameter *app-settings* (make-app-settings (ps:create hide-done-items false filter-text "")))))

(define-dispatchable-functions app-settings (app-settings)
  ((get-app-settings ()
                     app-settings)

   (initialize-app-settings (new-app-settings)
                            (setq app-settings new-app-settings))
   
   (update-app-settings (updated-app-settings)
                        (setq app-settings updated-app-settings))))

(define-for-ps app-settings (op &rest parameters)
  "Handle boilerplate function calls to consume app settings"
  (apply (funcall *app-settings* op) parameters))

(define-for-ps get-app-settings-from-server (&optional call-back)
  "define callback and make call to get app settings from server, then re-render html elements"
  (flet ((local-call-back ()
           (let ((server-app-settings (ps:chain -j-s-o-n (parse (@ this response-text)))))
             (unless (@ server-app-settings filter-text)
               (setf (@ server-app-settings filter-text) ""))
             (app-settings 'initialize-app-settings server-app-settings)
             (render-app-settings)
             (render-todo-filter)
	     (render-tag-filter-ui)
             (call-back)
             t)))
    (get-from-server *app-settings-api-endpoint* local-call-back)))

(define-for-ps update-app-settings (&key (can-re-render t))
  "update app settings on client and server and re-render html elements"
  (let ((input-hide-done-items (@ (ps:chain document (get-element-by-id "hide-done")) checked))
        (input-filter-text (@ (ps:chain document (get-element-by-id "todo-filter-text")) value))
        (app-settings (app-settings 'get-app-settings)))
    (setf (@ app-settings hide-done-items) input-hide-done-items)
    (setf (@ app-settings filter-text) input-filter-text)
    (setf (@ app-settings selected-filter-tag-ids) (get-currently-selected-tag-ids "filter-")) ;; TODO - can update this to use new object
    (setf (@ app-settings filter-tag-match-type) *filter-tag-match-type*)
    (send-to-server *app-settings-api-endpoint* "PUT" app-settings)
    (app-settings 'update-app-settings app-settings)
    (when can-re-render
      (render-filter-tag-todos "filter-"))))
