;;;; todo-project.lisp

(in-package #:todo-project)

(import-macros-from-lisp 'with-html-elements)

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

    (defparameter *todo-checkbox* "todo-check")
    (defparameter *todo-label* "todo-label")

    (defun clear-field (field)
      (setf (chain field value) "")
      t)
    
    (defun clear-children (parent-element)
      (while (chain parent-element (has-child-nodes))
        (chain parent-element (remove-child (@ parent-element first-child)))))

    (defun get-next-index (todo-list)
      (let ((id-list (chain todo-list (map #'(lambda (todo) (@ todo id)))))
            (max-fn (@ -Math max)))
        (if (length id-list)
            (+ 1 (chain max-fn (apply null id-list)))
            1)))

    (defun add-todo (evt)
      (chain evt (prevent-default))
      (let* ((todo (chain document (get-element-by-id "todo-content")))
             (todo-text (chain todo value))
             (next-id (get-next-index todo-list)))
        (chain todo-list (push (create text todo-text done false id next-id)))
        (clear-field todo)
        (render-todo-list todo-list)
        t))
    
    (defun update-app-settings ()
      (let ((input-hide-done-items (@ (chain document (get-element-by-id "hide-done")) checked)))
        (setf *hide-done-items* input-hide-done-items)
        (render-todo-list todo-list)))

    (var *hide-done-items* false)

    (defun render-app-settings ()
      (let ((parent-element (chain document (get-element-by-id "app-settings"))))
        (jfh-web::with-html-elements
            (div (span (input (id . "hide-done") (type . "checkbox") (onclick . "(update-app-settings)")) "Hide Done Items.")))))

    (defun init ()
      (render-app-settings)
      (setf add-button (chain document
                              (get-element-by-id "todo-add-btn")))
      (chain add-button
             (add-event-listener "click" add-todo false)))

    (defun update-todo (index todo-id)
      (let* ((checked (@ (chain document (get-element-by-id (+ "todo-check" index))) checked))
             (label (chain document (get-element-by-id (+ "todo-label" index))))
             (todo-list-index (@ (chain todo-list (find-index #'(lambda (todo) (= todo-id (@ todo id)))))))
             (todo-item (aref todo-list todo-list-index)))
        (if checked
            (setf (@ label style "text-decoration") "line-through")
            (setf (@ label style "text-decoration") ""))
        (setf (@ todo-item done) checked))
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
        (chain todo-list
               (filter
                #'(lambda (todo)
                    (or (not *hide-done-items*)
                        (not (@ todo done)))))
               (map
                #'(lambda (todo index)
                    (let ((todo-checkbox-id (+ *todo-checkbox* index))
                          (todo-label-id (+ *todo-label* index)))
                      (jfh-web::with-html-elements
                          (tr
                           (td
                            (input
                             (id . "(chain todo-checkbox-id (to-string))")
                             (type . "checkbox")
                             (onclick . "(update-todo (chain index (to-string)) (@ todo id)))")
                             (checked . "(@ todo done)"))
                            (label (id . "(chain todo-label-id (to-string))") (html-for . "(chain todo-checkbox-id (to-string))") "(@ todo text)"))))
                      t))))))

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
                     (str (jfh-web:define-ps-with-html-macro))
                     (str (todo-list-interaction))))
           (:body
            (:div :id "app-settings")
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
