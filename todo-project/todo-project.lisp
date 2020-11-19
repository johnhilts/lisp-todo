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

    (defun send-todo-item-to-server (todo-item http-method)
      "Do AJAX request using PUSH or PUT"
      (let ((xml-http (new (-x-m-l-http-request)))
             (the-url "/todo-data"))
        (chain xml-http (open http-method the-url))
        (chain xml-http (set-request-header "Content-Type" "application/json;charset=UTF-8"))
        (chain xml-http (send (chain -j-s-o-n (stringify todo-item)))))
      t)

    (defun send-new-todo-item-to-server (todo-item)
      (send-todo-item-to-server todo-item "PUSH"))

    (defun send-updated-item-to-server (todo-item)
      (send-todo-item-to-server todo-item "PUT"))
    
    (defun add-todo (evt)
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
    
    (defun update-app-settings ()
      (let ((input-hide-done-items (@ (chain document (get-element-by-id "hide-done")) checked)))
        (setf *hide-done-items* input-hide-done-items)
        (render-todo-list todo-list)))

    (var *hide-done-items* false)

    (defun render-app-settings ()
      (let ((parent-element (chain document (get-element-by-id "app-settings"))))
        (jfh-web::with-html-elements
            (div (span (input (id . "hide-done") (type . "checkbox") (onclick . "(update-app-settings)")) "Hide Done Items.")))))

    (defun get-todo-list-from-server ()
      (flet ((req-listener ()
               (let ((server-todo-list (chain -j-s-o-n (parse (@ this response-text)))))
                 (render-todo-list server-todo-list)
                 (setf todo-list server-todo-list)
                 t)))
        (let ((o-req (new (-x-m-l-http-request))))
          (chain o-req (add-event-listener "load" req-listener))
          (chain o-req (open "GET" "/todo-data"))
          (chain o-req (send)))))
    
    (defun init ()
      (render-app-settings)
      (get-todo-list-from-server)
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
        (setf (@ todo-item done) checked)
        (send-updated-item-to-server todo-item))
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
                            (label
                             (id . "(chain todo-label-id (to-string))")
                             (for . "(chain todo-checkbox-id (to-string))")
                             (style . "(if (@ todo done) \"text-decoration: line-through;\" \"\")") "(@ todo text)"))))
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
