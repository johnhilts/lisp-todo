
(in-package #:todo-project)

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
  (fetch-or-create-data *todo-file-path*))

(defun get-todo-list ()
  "get todo list and encode as json"
  (encode-multiple-plists-to-json-as-string (fetch-or-create-todos)))

(defun todo-data-get (id)
  "get todo by ID or entire list"
  (if id
      (get-todo (parse-integer id))
      (get-todo-list)))

(define-data-update-handler todo-data-add (model)
    "add todo data to persisted data"
  (let ((new-id (getf model :id))
        (existing-todos (fetch-or-create-todos)))
    (write-complete-file *todo-file-path* (append existing-todos (list model)))
    (json:encode-json-to-string (list new-id))))

(define-data-update-handler todo-data-update (model)
  "update todo data and persisted data"
  (let* ((update-id (getf model :id))
         (existing-todos (fetch-or-create-todos))
         (updated-item-position (position-if #'(lambda (e) (= (getf e :id) update-id)) existing-todos))
         (updated-todos (splice-and-replace-item-in-list existing-todos model updated-item-position)))
    
    (write-complete-file *todo-file-path* updated-todos)
    (json:encode-json-to-string (list update-id))))
