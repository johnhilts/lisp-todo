(in-package #:todo-project)

(defun client-todo ()
  "define client side functions to handle todos"
  (ps
    (defvar todo-list ([]))))

(define-for-ps send-new-todo-item-to-server (todo-item)
  "save new todo on server"
  (send-to-server *todo-api-endpoint* "POST" todo-item))

(define-for-ps send-updated-todo-item-to-server (todo-item)
  "save updated todo on server"
  (send-to-server *todo-api-endpoint* "PUT" todo-item))

(define-for-ps delete-todo-item-on-server (delete-id-object)
  "save updated todo on server"
  (send-to-server *todo-api-endpoint* "DELETE" delete-id-object))
    
(define-for-ps add-todo (evt)
  "add todo on client and server and re-render html elements"
  (chain evt (prevent-default))
  (let* ((todo (chain document (get-element-by-id "todo-content")))
         (todo-text (chain todo value)))
    (flet ((call-back ()
             (let* ((next-id (get-next-index todo-list))
                    (todo-item  (create text todo-text done false id next-id)))
               (chain todo-list (push todo-item))
               (clear-field todo)
               (render-todo-list todo-list)
               (send-new-todo-item-to-server todo-item))
             t))
      (get-todo-list-from-server #'(lambda () (call-back todo todo-text)))
      t)))

(define-for-ps get-todo-list-from-server (&optional optional-call-back)
  "define callback and get todo list from server and re-render html elements"
  (flet ((call-back ()
           (let ((server-todo-list (chain -j-s-o-n (parse (@ this response-text)))))
             (render-todo-list server-todo-list)
             (setf todo-list server-todo-list)
             (when optional-call-back
               (optional-call-back))
             t)))
    (get-from-server *todo-api-endpoint* call-back)))

(define-for-ps update-todo (index todo-id)
      "update todo on client and server and re-render html elements"
      (let* ((checked (@ (chain document (get-element-by-id (+ "todo-check" index))) checked))
             (label (chain document (get-element-by-id (+ "todo-label" index))))
             (todo-list-index (@ (chain todo-list (find-index #'(lambda (todo) (= todo-id (@ todo id)))))))
             (todo-item (aref todo-list todo-list-index)))
        (if checked
            (setf (@ label style "text-decoration") "line-through")
            (setf (@ label style "text-decoration") ""))
        (setf (@ todo-item done) checked)
        (send-updated-todo-item-to-server todo-item))
      t)

(define-for-ps update-todo-from-edit (todo)
  "update todo on client and server and re-render html elements"
  (send-updated-todo-item-to-server todo)
  (render-todo-list todo-list)
  t)

(define-for-ps delete-todo-by-id (delete-id)
  "delete todo on client and server and re-render html elements"
  (delete-todo-item-on-server (create id delete-id))
  (let ((delete-item-index (chain todo-list (find-index #'(lambda (todo) (= (@ todo id) delete-id))))))
    (chain todo-list (splice delete-item-index 1)))
  (render-todo-list todo-list)
  t)

    
