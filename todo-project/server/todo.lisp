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

(defun fetch-or-create-todos (&optional (get-user-data-path #'get-user-data-path) )
  "get todo from persisted data"
  (let ((user-data-path (if get-user-data-path (funcall get-user-data-path nil :by :login) "")))
    (fetch-or-create-data (concatenate 'string user-data-path "/" *todo-file-name*))))

(defun get-todo-list ()
  "get todo list and encode as json"
  (encode-multiple-plists-to-json-as-string (fetch-or-create-todos)))

(defun todo-data-get (id)
  "get todo by ID or entire list"
  (if id
      (get-todo (parse-integer id))
      (get-todo-list)))

(defun get-next-todo-index (todo-list)
  "calculate next index for todo list"
  (let ((id-list (mapcar #'(lambda (todo) (getf todo :id)) todo-list)))
    (if (null id-list)
        1
        (+ 1 (apply #'max id-list)))))

(defun transform-lines-to-todos (lines start-new-id)
  "transform lines (string ending in #\Newline) into a list of todo items"
  (let ((split-lines (split-string-by #\Newline (remove #\Return lines))))
    (reduce #'(lambda (acc cur)
                (let ((todo (list (list :text cur :done 0 :id (+ start-new-id (length acc))))))
                  (append acc todo)))
            split-lines :initial-value nil)))

(defun import-lines-into-todo-list (lines)
  "orchestrator called by web handler to take input and output it in desired form"
  (let* ((existing-todos (fetch-or-create-todos))
         (new-id (get-next-todo-index existing-todos))
         (user-data-path (get-user-data-path nil :by :login))
         (new-todos (transform-lines-to-todos lines new-id)))
    (write-complete-file (concatenate 'string user-data-path "/" *todo-file-name*) (append existing-todos new-todos))
    (mapcar #'(lambda (e) (getf e :id)) new-todos)))

(define-data-update-handler todo-data-add (model)
    "add todo data to persisted data"
  (let ((new-id (getf model :id))
        (existing-todos (fetch-or-create-todos))
        (user-data-path (get-user-data-path nil :by :login)))
    (write-complete-file (concatenate 'string user-data-path "/" *todo-file-name*) (append existing-todos (list model)))
    (json:encode-json-to-string (list new-id))))

(define-data-update-handler todo-data-update (model)
  "update todo data and persisted data"
  (let* ((update-id (getf model :id))
         (existing-todos (fetch-or-create-todos))
         (updated-item-position (position-if #'(lambda (e) (= (getf e :id) update-id)) existing-todos))
         (updated-todos (splice-and-replace-item-in-list existing-todos model updated-item-position))
         (user-data-path (get-user-data-path nil :by :login)))
    
    (write-complete-file (concatenate 'string user-data-path "/" *todo-file-name*) updated-todos)
    (json:encode-json-to-string (list update-id))))

(define-data-update-handler todo-data-delete (model)
  "delete todo by ID"
  (let ((delete-id (getf model :id)))
    (when delete-id
      (let* ((existing-todos (fetch-or-create-todos))
             (deleted-item-position (position-if #'(lambda (e) (= (getf e :id) delete-id)) existing-todos))
             (updated-todos (splice-and-remove-item-in-list existing-todos deleted-item-position))
             (user-data-path (get-user-data-path nil :by :login)))
        (write-complete-file (concatenate 'string user-data-path "/" *todo-file-name*) updated-todos)
        (json:encode-json-to-string (list delete-id))))))

