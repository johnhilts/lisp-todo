(in-package #:todo-project)

(defun client-todo ()
  "define client side functions to handle todos"
  (ps
    (defvar todo-list ([]))

    (defun send-new-todo-item-to-server (todo-item)
      "save new todo on server"
      (send-to-server "/todo-data" "PUSH" todo-item))

    (defun send-updated-todo-item-to-server (todo-item)
      "save updated todo on server"
      (send-to-server "/todo-data" "PUT" todo-item))
    
    (defun add-todo (evt)
      "add todo on client and server and re-render html elements"
      (chain evt (prevent-default))
      (let* ((todo (chain document (get-element-by-id "todo-content")))
             (todo-text (chain todo value))
             (next-id (get-next-index todo-list))
             (todo-item  (create text todo-text done false id next-id)))
        (chain todo-list (push todo-item))
        (clear-field todo)
        (render-todo-list todo-list)
        (send-new-todo-item-to-server todo-item)
        t))

    (defun get-todo-list-from-server ()
      "define callback and get todo list from server and re-render html elements"
      (flet ((call-back ()
               (let ((server-todo-list (chain -j-s-o-n (parse (@ this response-text)))))
                 (render-todo-list server-todo-list)
                 (setf todo-list server-todo-list)
                 t)))
        (get-from-server "/todo-data" call-back)))

    (defun update-todo (index todo-id)
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
      t)))

    
