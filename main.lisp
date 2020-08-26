;; install these: (ql:quickload '(cl-who hunchentoot parenscript cl-json))
(defpackage :todo-webapp
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :todo-webapp)

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

(defun cons-pair-p (possible-cons)
  (and (consp possible-cons) (atom (cdr possible-cons)) (not (null (cdr possible-cons)))))

(defmacro with-html-elements (elements)
  "
input: html elements as sexprs
output: javascript that creates the elements in DOM based on the sexprs
"
  (labels
      ((process-tag-r (element &optional (parent nil parent-supplied-p))
         (let* ((tag (car element))
                (parent-element (gensym (concatenate 'string (string-downcase tag) "Element")))
                (parent-element-parameter (if parent-supplied-p parent (make-symbol "parent-element"))))
           (cons
            `(let ((,parent-element (create-an-element ,parent-element-parameter ,(string tag)))))
            (mapcar
             #'(lambda (e)
                 (cond
                   ((cons-pair-p e)
                    `(set-an-attribute ,parent-element ,(string (car e))  ,(string (cdr e))))
                   ((stringp e)
                    `(set-text-node ,parent-element ,e))
                   ((listp e)
                    `(progn
                       ,@(process-tag-r e parent-element)))
                   ((atom e)
                    `(set-text-node ,parent-element ,e))))
             (cdr element))))))
    `(ps
       (defun create-elements (parent-element)
         ,@(process-tag-r elements)
         parent-element)
       (let ((parent-element (chain document (get-element-by-id "todo-list"))))
         (create-elements parent-element)))))

(import-macros-from-lisp 'with-html-elements)

(defun setup-client-info ()
  (ps
    (defun init-info ()
      (let ((todo-list ([])))
        todo-list))
    (defvar todo-list (init-info))))

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
                     (str (ps (defun add-todo (evt)
                                (chain evt (prevent-default))
                                (let* ((todo (chain document (get-element-by-id "todo-content")))
                                       (todo-text (chain todo value)))
                                  (chain todo-list (push todo-text))))
                              (defun init ()
                                (setf add-button (chain document
                                                        (get-element-by-id "todo-add-btn")))
                                (chain add-button
                                       (add-event-listener "click" add-todo false)))
                              (defun render-todo-list ()
                                (let ((todo-list-div (chain document (get-element-by-id "todo-list"))))
                                  (chain todo-list (map
                                                    #'(lambda (todo)
                                                        (with-html-elements
                                                            (tr (td todo))))))))
                              (setf (chain window onload) init)))))
           (:body
            (:div
             (:h1 "Todo List"
                  (:div
                   (:input :id "todo-check" :type "checkbox" :onclick (ps-inline (alert "You clicked the checkbox!")))
                   (:textarea :id "todo-content" :placeholder "Enter Todo info here.")
                   (:button :id "todo-add-btn" "Add"))
                  (:div :id "todo-list"
                        (:table
                         (:tr (:td "To-do"))))))))))

(define-easy-handler (todo-page :uri "/todos") ()
  (make-todo-page))

(defparameter *the-http-server* (start-server 5050))

(defun stop-server (server)
  (stop server))

(start-web-app)

#||
;;Send me to the world, in JSON.
(hunchentoot:define-easy-handler (say-me :uri "/me") ()
  (setf (hunchentoot:content-type*) "application/json")
  (let ((me (make-instance 'people
                           :name "John Hilts"
                           :language "Lisp"
                           :bio "I'm learning and loving common lisp!")))
    (json:encode-json-to-string me)))

;;Dynamic build a people, and response back in JSON.
(hunchentoot:define-easy-handler (say-you :uri "/you") (name)
  (setf (hunchentoot:content-type*) "application/json")
  (json:encode-json-to-string
   (make-instance
    'people
    :name name
    :language "English"
    :bio (format nil "I am ~a's colon. I get cancer, I kill ~a. " name name))))


;; photo lister prototype
(hunchentoot:define-easy-handler (list-photos :uri "/api/photos") ()
  (setf (hunchentoot:content-type*) "application/json")
  (let ((photos (list "photo1.jpg" "photo2.jpg" "photo3.jpg")))
    (json:encode-json-to-string photos)))

(defun make-content-viewer-page ()
  (let ((photos (list "photo1.jpg" "photo2.jpg" "photo3.jpg")))
    (with-html-output-to-string
        (*standard-output* nil :prologue t :indent t)
      (:html :lang "en"
             (:head
              (:meta :charset "utf-8")
              (:title "List Photos")
              (:link :type "text/css"
                     :rel "stylesheet"
                     :href "/proto-type.css")
              (:script :type "text/javascript"
                       (str "")))
             (:body
              (:div :id "header"
                    (:img :src "/logo.jpg"
                          :alt "Commodore 64"
                          :class "logo"))
              (:div "List Photos"
                    ;; (mapcar #'(lambda (photo)
                    (dolist (photo photos)
                                (htm 
                                 (:div
                                  ((:label) (fmt "~a" photo))
                                  (:div
                                   (:input :type "button" :value "Click me!" :onclick (ps #'(lambda () (alert "You clicked me!")))))))) ; photos
                    )))))))

(define-easy-handler (content-viewer :uri "/view/photos") ()
  (make-content-viewer-page))
||#
