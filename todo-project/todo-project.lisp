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
                     (str (ajax))
                     (str (client-util))
                     (str (client-todo))
                     (str (client-app-settings))
                     (str (client-ui))))
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

(defun test-json-data ()
  (list
   (list :id 1 :done T :text "Go Shopping")
   (list :id 2 :done 0 :text "Water Plants")))


(defun string-replace (string search replace)
  (labels
      ((replace-r (string search replace)
         (if (zerop (length string))
             string
             (let ((search-position (search search string)))
               (if (null search-position)
                   string
                   (concatenate 'string
                                (subseq string 0 search-position)
                                replace
                                (replace-r (subseq string  (+ search-position (length search))) search replace)))))))
    (replace-r string search replace)))

(defun encode-plist-to-json-as-string (plist)
  (string-replace (json:encode-json-plist-to-string plist) "\"done\":0," "\"done\":false,"))

(defun encode-multiple-plists-to-json-as-string (plists)
  (labels
      ((concat-plists (plists)
         (cond
           ((= 1 (length plists))
            (encode-plist-to-json-as-string (car plists)))
           (t (concatenate 'string
                           (encode-plist-to-json-as-string (car plists))
                           ", "
                           (concat-plists (cdr plists)))))))

    (if plists
        (concatenate 'string
                     "["
                     (concat-plists plists)
                     "]")
        "[]")))

#||
;; test in REPL
(format t "~s~%" (encode-json-multiple-plist-to-string (test-json-data)))
(format t "~a~%" (get-todo 2))
(format t "~a~%" (get-todo 1))
(format t "~a~%" (get-todo-list))
(string-replace "ABCDEFGHI" "CDE" "XYZ")
(string-replace "ABCDEFGHI" "CDE" "WXYZ")
(string-replace (encode-json-multiple-plist-to-string (test-json-data)) "done" "done-zo")
(string-replace (encode-json-multiple-plist-to-string (test-json-data)) "\"done\":0," "\"done\":false,")
(format t "~s~%" (read-complete-file "./test3.sexp"))
(write-complete-file "./test3.sexp" (list (list :text "Go Shopping" :done t :id 1) (list :text "Clean house" :done nil :id 2) (list :text "Make lunch" :done nil :id 3)))
(format t "~s~%" (read-complete-file-at-once "./test4.sexp"))
(write-complete-file-at-once "./test4.sexp" (list (list :text "Go Shopping" :done t :id 1) (list :text "Clean house" :done nil :id 2) (list :text "Make lunch" :done nil :id 3)))
||#

(defun read-complete-file-by-line (path)
  (let ((file ()))
    (with-open-file (in path)
      (do ((line (read in nil) (read in nil)))
          ((null line) file)
        (setf file (append file (list line)))))))

(defun write-complete-file-by-line (path list)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (line list)
      (print line out))))

(defun read-complete-file (path)
  (with-open-file (in path :if-does-not-exist :create)
    (read in nil)))

(defun write-complete-file (path list)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (prin1 list out))) ;; print is just like prin1, except it precedes each output with a line break, and ends with a space

(defun convert-nil-to-zero (keyword value)
  (list keyword (if (and (equal :done keyword) (null value)) 0 value)))

(defun join-pairs (acc cur)
  (append
   acc
   (convert-nil-to-zero
    (car cur)
    (cdr cur))))

(defun iterate-through-pairs (acc cur)
  (append
        acc
        (list (reduce #'join-pairs cur :initial-value ()))))

(defun fetch-or-create-settings ()
  (let ((settings (read-complete-file "./setting-list.sexp")))
    (if settings
        settings
        (list :hide-done-items 0))))

(defun get-setting ()
  ;; todo - put the string replace inside of encode-plist-to-json-as-string (it's my function!)
  (string-replace (encode-plist-to-json-as-string (fetch-or-create-settings)) "\"hideDoneItems\":0" "\"hideDoneItems\":false"))

(defun setting-data-get ()
  (get-setting))

(defun convert-input-to-setting (input)
  (reduce #'join-pairs  input :initial-value ()))

(defun setting-data-update (raw-data)
  (let ((updated-setting (convert-input-to-setting (json:decode-json-from-string raw-data))))
    (write-complete-file "./setting-list.sexp" updated-setting)
    (json:encode-json-to-string updated-setting)))

(defun convert-input-to-todo-list (input)
  (reduce #'iterate-through-pairs input :initial-value ()))

(defun convert-input-to-todo (input)
  (reduce #'join-pairs  input :initial-value ()))

(defun get-todo-by-id (id)
  (let ((todos (test-json-data)))
         (find-if
          #'(lambda (e)
              (let ((search-id (getf e :id)))
                (equal search-id id))) todos)))

(defun get-todo (id)
  (let ((todo (get-todo-by-id id)))
    (encode-plist-to-json-as-string todo)))

(defun fetch-or-create-todos ()
   (read-complete-file "./todo-list.sexp"))

(defun get-todo-list ()
  (encode-multiple-plists-to-json-as-string (fetch-or-create-todos)))

(defun todo-data-get (id)
  (if id
      (get-todo (parse-integer id))
      (get-todo-list)))

(defun todo-data-add (raw-data)
  (let* ((new-todo (convert-input-to-todo (json:decode-json-from-string raw-data)))
         (new-id (getf new-todo :id))
         (existing-todos (fetch-or-create-todos)))
    (write-complete-file "./todo-list.sexp" (append existing-todos (list new-todo)))
    (json:encode-json-to-string (list new-id))))

(defun todo-data-update (raw-data)
  (let* ((update-todo (convert-input-to-todo (json:decode-json-from-string raw-data)))
         (update-id (getf update-todo :id))
         (existing-todos (fetch-or-create-todos))
         (non-update-todos (remove-if #'(lambda (e) (= update-id (getf e :id))) existing-todos))
         (updated-todos (append (list update-todo) non-update-todos)))
    
    (write-complete-file "./todo-list.sexp" updated-todos)
    (json:encode-json-to-string (list update-id))))

(define-easy-handler (todo-data :uri "/todo-data") (id)
  (setf (content-type*) "application/json")
  (let* ((raw-data  (raw-post-data :force-text t))
         (verb (request-method *request*)))
    (case verb
      (:put
       (todo-data-update raw-data))
      (:post
       (todo-data-add raw-data))
      (:get
       (todo-data-get id)))))

(define-easy-handler (setting-data :uri "/setting-data") ()
  (setf (content-type*) "application/json")
  (let* ((raw-data  (raw-post-data :force-text t))
         (verb (request-method *request*)))
    (case verb
      (:put
       (setting-data-update raw-data))
      (:get
       (setting-data-get)))))

(convert-input-to-todo-list  '(((:TEXT . "Go Shopping") (:DONE . T) (:ID . 1))
                               ((:TEXT . "Water some plants") (:DONE) (:ID . 2))))

(car (convert-input-to-todo-list  '(((:TEXT . "Go Shopping") (:DONE . T) (:ID . 1)))))

(reduce #'join-pairs  '((:TEXT . "Go Shopping") (:DONE . T) (:ID . 1)) :initial-value ())

(defun handle-test-get (id)
  (format t "The ID is: ~d~%" id))

(defun handle-test-post (raw-data)
  (let* ((json-data (json:decode-json-from-string raw-data))
         (id (car json-data))
         (name (cadr json-data)))
    (format t "The ID is ~d, and the name is ~a~%" id name)))

(define-easy-handler (test-verbs :uri "/test-verbs") (id)
  (setf (content-type*) "application/json")
  (let* ((raw-data  (raw-post-data :force-text t))
         (verb (request-method *request*)))
    ;; (format t "~&verb: ~s~%id=~a~%name=~a~%raw post data: ~a~%lisp: ~s~%" (request-method *request*) id name raw-data json-data)
    (case verb
      (:post
       (format t "post!~%")
       (handle-test-post raw-data))
      (:put
       (format t "put!~%")
       (handle-test-post raw-data))
      (:get
       (format t "get!~%")
       (handle-test-get id))
      (otherwise (format t "no matches!~%")))))

(defparameter *the-http-server* (start-server 5050))

(defun stop-server (server)
  (stop server))

(start-web-app)
