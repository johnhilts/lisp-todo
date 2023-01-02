(in-package #:todo-project)

(defun client-todo ()
  "define client side functions to handle todos"
  (ps:ps
   
   (defparameter *todos* (make-todos ([])))
   (defparameter *tags* (make-tags ([])))
   (defparameter *tag-todos* (make-tag-todos ([])))))

(ps:ps
  (defmacro with-callback (fn &body body)
    `(,(car fn) ,@(cdr fn) #'(lambda (),@body))))

(define-dispatchable-functions todos (todos)
  ((get-todos ()
              todos)

   (initialize-todos (new-todos)
                     (setq todos new-todos))
   
   (add-todo (todo)
             (push* todo todos))

   (delete-todo (todo-id)
                (let ((delete-item-index (position-if* #'(lambda (todo) (= (@ todo id) todo-id)) todos)))
                  (ps:chain todos (splice delete-item-index 1))))))

(define-for-ps todo-items (op &rest parameters)
  "Handle boilerplate function calls to consume the list of todos"
  (apply (funcall *todos* op) parameters))

(define-for-ps get-all-todos ()
  "Get list of all todos"
  (todo-items 'get-todos))

(define-for-ps get-todos-filtered-by-tags ()
  "Get todo items filtered by tags"
  *todos-filtered-by-tags*)

(define-for-ps set-todos-filtered-by-tags (todos-filtered-by-tags)
  "Update todo items filtered by tags"
  (setf *todos-filtered-by-tags* todos-filtered-by-tags))

(define-for-ps get-todos-filtered-by-tags-for-single-todo-id (todos-filtered-by-tags)
  "Filter tags by todo ID"
  (ps:chain todos-filtered-by-tags (some #'(lambda (filtered-todo-id) (= filtered-todo-id (@ todo id))))))

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
      (let* ((next-id (get-next-index (get-all-todos)))
             (todo-item  (ps:create text todo-text done false id next-id)))
        (todo-items 'add-todo todo-item)
        (clear-field todo)
        (render-todo-list (get-all-todos))
        (send-new-todo-item-to-server todo-item)
        (add-associate-tags-to-todo next-id *selected-tag-ids*)
        (funcall *show-tag-content-handler*)))
    ;; calling .focus() outside of the callback *synchronously* so that the keyboard will appear when using iOS
    (ps:chain todo (focus)))
  t)

(define-for-ps get-todo-list-from-server (&optional optional-call-back)
  "define callback and get todo list from server and re-render html elements"
  (with-callback
      (get-from-server *todo-api-endpoint*)
    (let ((server-todo-list (ps:chain -j-s-o-n (parse (@ this response-text)))))
      (render-todo-list server-todo-list)
      (todo-items 'initialize-todos server-todo-list)
      (when optional-call-back
        (optional-call-back))))
  t)

(define-for-ps update-todo (index todo-id)
      "update todo on client and server and re-render html elements"
      (let* ((todos (get-all-todos))
             (checked (@ (ps:chain document (get-element-by-id (+ "todo-check" index))) checked))
             (label-pre (ps:chain document (query-selector (+ "#todo-label" index " pre"))))
             (todo-list-index (position-if* #'(lambda (todo) (= todo-id (@ todo id))) todos))
             (todo-item (aref todos todo-list-index)))
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
  (edit-associate-tags-to-todo (ps:@ todo id) *selected-tag-ids*)
  (render-todo-list (get-all-todos))
  t)

(define-for-ps delete-todo-by-id (delete-id)
  "delete todo on client and server and re-render html elements"
  (delete-todo-item-on-server (ps:create id delete-id))
  (todo-items 'delete-todo delete-id)
  (render-todo-list (get-all-todos))
  t)
    
