
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
  (or
   (and (consp possible-cons) (atom (cdr possible-cons)))
   (and (consp possible-cons) (listp (cdr possible-cons)) (equal '~f (cadr possible-cons)))))

(defun define-ps-with-html-macro ()
  (ps
    (defun create-an-element (parent-element tag)
      (let ((new-element (chain document (create-element tag))))
        (chain parent-element (append-child new-element))
        new-element))
    (defun set-an-attribute (parent-element key value)
      (chain parent-element (set-attribute key value)))
    (defun set-text-node (parent-element text)
      (let ((a-text-node (chain document (create-text-node text))))
        (chain parent-element (append-child a-text-node))))
    (defmacro with-html-elements (elements)
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
                        (if (and (listp (cdr e)) (equal '~f (cadr e)))
                            `(set-an-attribute ,parent-element ,(string (car e))  ,(chain (subseq (cdr e) 1))) ;; (subseq (cdr e) 2))
                            `(set-an-attribute ,parent-element ,(string (car e))  ,(string (cdr e)))))
                       ((stringp e)
                        `(set-text-node ,parent-element ,e))
                       ((listp e)
                        `(progn
                           ,@(process-tag-r e parent-element)))
                       ((symbolp e)
                        `(set-text-node ,parent-element ,e))))

                 (cdr element))))))
        `(progn ,@(process-tag-r elements))))))

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
                          #'(lambda (todo)
                              (with-html-elements
                                  (tr (td (input (id . "todo-check") (type . "checkbox") (onclick . (~f (alert "You clicked the checkbox!")))) todo)))
                              t)))))
    
    (setf (chain window onload) init))))

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
