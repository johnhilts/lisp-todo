
(defun test-json-data ()
  (list
   (list :id 1 :done T :text "Go Shopping")
   (list :id 2 :done 0 :text "Water Plants")))

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
