;;;; todo-project.lisp

(in-package #:todo-project)

(defun start-server (port)
  (restart-case (start (make-instance 'easy-acceptor :port port))
    (re-start-server ()
      :report "Restart Web Server"
      (stop-server *the-http-server*)
      (start-server port))))

(setf (html-mode) :html5)

;; allow parenscript and cl-who to work together
(setf *js-string-delimiter* #\")

(defun start-web-app ()
  (publish-static-content))

(defun publish-static-content ()
  (push (create-static-file-dispatcher-and-handler
         "/styles.css" "static/styles.css") *dispatch-table*))

(defun setup-client-info ()
  (ps
    (defun init-info ()
      (let ((todo-list ([])))
        todo-list))
    (defvar todo-list (init-info))))

(defun todo-list-interaction ()
  (ps

    (defun clear-field (field)
      (setf (chain field value) "")
      t)
    
    (defun clear-children (parent-element)
      (while (chain parent-element (has-child-nodes))
        (chain parent-element (remove-child (@ parent-element first-child)))))

    (defun add-todo (evt)
      (chain evt (prevent-default))
      (let* ((todo (chain document (get-element-by-id "todo-content")))
             (todo-text (chain todo value)))
        (chain todo-list (push todo-text))
        (clear-field todo)
        (render-todo-list todo-list)
        t))
    
    (defun init ()
      (setf add-button (chain document
                              (get-element-by-id "todo-add-btn")))
      (chain add-button
             (add-event-listener "click" add-todo false)))

    (defun update-todo (index)
      (let* ((checked (@ (chain document (get-element-by-id (+ "todo-check" index))) checked))
             (label (chain document (get-element-by-id (+ "todo-label" index)))))
        (if checked
            (setf (@ label style "text-decoration") "line-through")
            (setf (@ label style "text-decoration") "")))
      t)
    
    (defun render-todo-list (todo-list)
      (let* ((todo-list-table-body (chain document (get-element-by-id "todo-list-body")))
             (parent-element todo-list-table-body)
             (column-header (chain document (get-element-by-id "todo-list-column-header")))
             (count (length todo-list))
             (use-plural-form (or (> 1 count) (= 0 count))))
        (clear-children parent-element)
        (setf (chain column-header inner-text)
              (if use-plural-form "To-do Items" "To-do Item"))
        (chain todo-list (map
                          #'(lambda (todo index)
                              (let ((checkbox-id (+ "todo-check" index))
                                    (label-id (+ "todo-label" index)))                                
                                (with-html-elements
                                    (tr (key . index)
                                        (td
                                         (input (id . "todo-check") (type . "checkbox") (onclick . "(+ \"updateTodo(\" (chain index (to-string)) \")\")"))
                                         (input (id . "test-check") (type . "button") (onclick . "(updateTodo 123)"))
                                  (label (id . "todo-label") todo))))
                          
                          (let ((todo-check-box (chain document (get-element-by-id "todo-check")))
                                (todo-label (chain document (get-element-by-id "todo-label"))))
                            (setf (@ todo-check-box id) checkbox-id
                                  (@ todo-label id) label-id
                                  (@ todo-label html-for) checkbox-id)
                            ))

                          t)))))

    (setf (chain window onload) init)))

(defun make-todo-page ()
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:title "Todo List")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/styles.css")
            (:script :type "text/javascript"
                     (str (setup-client-info))
                     (str (define-ps-with-html-macro))
                     (str (todo-list-interaction))))
           (:body
            (:div :id "div123"
                  "test area start"
                  (:br)
                  (:script :type "text/javascript"
                           (str
                            (ps
                              (let ((parent-element (chain document (get-element-by-id "div123"))))
                                (with-html-elements
                                    (table
                                     (tr
                                      (td
                                       (input (type . "button") (onclick . "alert('you clicked me!')"))))))))))
                  (:br)
                  "test area end")
            (:div
             (:h1 "Todo List"
                  (:div
                   (:textarea :id "todo-content" :placeholder "Enter Todo info here.")
                   (:button :id "todo-add-btn" "Add"))
                  (:div
                   (:table :id "todo-list"
                           (:thead (:th :id "todo-list-column-header" "To-do Items"))
                           (:tbody :id "todo-list-body" (:tr (:td "(To-do list empty)")))))))))))

(define-easy-handler (todo-page :uri "/todos") ()
  (make-todo-page))

(defparameter *the-http-server* (start-server 5050))

(defun stop-server (server)
  (stop server))

(start-web-app)

