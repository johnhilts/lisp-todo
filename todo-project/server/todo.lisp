
(in-package #:todo-project)

(defun convert-input-to-todo-list (input)
  "convert json to app-specific format (list version)"
  (reduce #'iterate-through-pairs input :initial-value ()))

(defun convert-input-to-todo (input)
  "convert json to app-specific format"
  (reduce #'join-pairs  input :initial-value ()))

(defun get-todo-by-id (id)
  "get todo by unique ID"
  (let ((todos (test-json-data)))
         (find-if
          #'(lambda (e)
              (let ((search-id (getf e :id)))
                (equal search-id id))) todos)))

(defun get-todo (id)
  "get todo by ID then convert to json"
  (let ((todo (get-todo-by-id id)))
    (encode-plist-to-json-as-string todo)))

(defun fetch-or-create-todos ()
  "get todo from persisted data"
   (read-complete-file "./todo-list.sexp"))

(defun get-todo-list ()
  "get todo list and encode as json"
  (encode-multiple-plists-to-json-as-string (fetch-or-create-todos)))

(defun todo-data-get (id)
  "get todo by ID or entire list"
  (if id
      (get-todo (parse-integer id))
      (get-todo-list)))

(defun todo-data-add (raw-data)
  "add todo data to persisted data"
  (let* ((new-todo (convert-input-to-todo (json:decode-json-from-string raw-data)))
         (new-id (getf new-todo :id))
         (existing-todos (fetch-or-create-todos)))
    (write-complete-file "./todo-list.sexp" (append existing-todos (list new-todo)))
    (json:encode-json-to-string (list new-id))))

(defun todo-data-update (raw-data)
  "update todo data and persisted data"
  (let* ((update-todo (convert-input-to-todo (json:decode-json-from-string raw-data)))
         (update-id (getf update-todo :id))
         (existing-todos (fetch-or-create-todos))
         (non-update-todos (remove-if #'(lambda (e) (= update-id (getf e :id))) existing-todos))
         (updated-todos (append (list update-todo) non-update-todos)))
    
    (write-complete-file "./todo-list.sexp" updated-todos)
    (json:encode-json-to-string (list update-id))))
