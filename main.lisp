;; install these: (ql:quickload '(cl-who hunchentoot parenscript cl-json))
(defpackage :todo-webapp
  (:use :cl :cl-who :hunchentoot :parenscript))

(in-package :todo-webapp)

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(setf (html-mode) :html5)

;; allow parenscript and cl-who to work together
(setf *js-string-delimiter* #\")


(defun publish-static-content ()
  (push (create-static-file-dispatcher-and-handler
         "/styles.css" "static/styles.css") *dispatch-table*))

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
                     (str (ps (defun add-todo (evt)
                                (chain evt (prevent-default))
                                (setf todo (chain document (get-element-by-id "todo-content")))
                                (alert (@ todo value)))
                              (defun init ()
                                (setf add-button (chain document
                                                      (get-element-by-id "todo-add-btn")))
                                (chain add-button
                                       (add-event-listener "click" add-todo false)))
                              (setf (chain window onload) init)))))
           (:body
            (:div
             (:h1 "Todo List"
                  (:div
                   (:input :id "todo-check" :type "checkbox" :onclick (ps-inline (alert "You clicked the checkbox!")))
                   (:textarea :id "todo-content" :placeholder "Enter Todo info here.")
                   (:button :id "todo-add-btn" "Add")
                   )))))))

(define-easy-handler (todo-page :uri "/todos") ()
  (make-todo-page))

(defparameter *the-http-server* (start-server 5050))

(defun stop-server (server)
  (stop server))

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
