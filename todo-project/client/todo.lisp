(in-package #:todo-project)

(defun client-todo ()
  "define client side functions to handle todos"
  (ps:ps
   (defvar todo-list ([]))
   (defparameter *tag-list* (list (create :id 1 :text "Shopping") (create :id 2 :text "Lunch") (create :id 3 :text "Electronics") (create :id 4 :text "Food")))))

(ps:ps
  (defmacro with-callback (fn &body body)
    `(,(car fn) ,@(cdr fn) #'(lambda (),@body))))

(define-for-ps send-new-todo-item-to-server (todo-item)
  "save new todo on server"
  (send-to-server *todo-api-endpoint* "POST" todo-item))

(define-for-ps send-updated-todo-item-to-server (todo-item)
  "save updated todo on server"
  (send-to-server *todo-api-endpoint* "PUT" todo-item))

(define-for-ps delete-todo-item-on-server (delete-id-object call-back)
  "save updated todo on server"
  (send-to-server *todo-api-endpoint* "DELETE" delete-id-object call-back))
    
(define-for-ps add-todo (evt)
  "add todo on client and server and re-render html elements"
  (ps:chain evt (prevent-default))
  (let* ((todo (ps:chain document (get-element-by-id "todo-content")))
         (todo-text (ps:chain todo value)))
    (with-callback
        (get-todo-list-from-server)
      (let* ((next-id (get-next-index todo-list))
             (todo-item  (ps:create text todo-text done false id next-id)))
        (ps:chain todo-list (push todo-item))
        (clear-field todo)
        (render-todo-list todo-list)
        (send-new-todo-item-to-server todo-item)))
    ;; calling .focus() outside of the callback *synchronously* so that the keyboard will appear when using iOS
    (ps:chain todo (focus)))
  t)

(define-for-ps get-todo-list-from-server (&optional optional-call-back)
  "define callback and get todo list from server and re-render html elements"
  (with-callback
      (get-from-server *todo-api-endpoint*)
    (let ((server-todo-list (ps:chain -j-s-o-n (parse (@ this response-text)))))
      (render-todo-list server-todo-list)
      (setf todo-list server-todo-list)
      (when optional-call-back
        (optional-call-back))))
  t)

(define-for-ps update-todo (index todo-id)
      "update todo on client and server and re-render html elements"
      (let* ((checked (@ (ps:chain document (get-element-by-id (+ "todo-check" index))) checked))
             (label-pre (ps:chain document (query-selector (+ "#todo-label" index " pre"))))
             (todo-list-index (@ (ps:chain todo-list (find-index #'(lambda (todo) (= todo-id (@ todo id)))))))
             (todo-item (aref todo-list todo-list-index)))
        (if checked
            (progn
              (setf (@ label-pre style "text-decoration") "line-through")
              (setf (@ label-pre style "color") "#888"))
            (progn
              (setf (@ label-pre style "text-decoration") "")
              (setf (@ label-pre style "color") "")))
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
  (delete-todo-item-on-server (ps:create id delete-id))
  (let ((delete-item-index (ps:chain todo-list (find-index #'(lambda (todo) (= (@ todo id) delete-id))))))
    (ps:chain todo-list (splice delete-item-index 1)))
  (render-todo-list todo-list)
  t)
    
