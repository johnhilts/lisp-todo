(in-package #:todo-project)

(defun fetch-or-create-tags (&optional (get-user-data-path #'get-user-data-path) )
  "get tag from persisted data"
  (let ((user-data-path (if get-user-data-path (funcall get-user-data-path nil :by :login) "")))
    (fetch-or-create-data (concatenate 'string user-data-path "/" *tag-file-name*))))

(defun get-tag-by-id (id)
  "get tag by unique ID"
  (let ((tags (test-json-data)))
         (find-if
          #'(lambda (e)
              (let ((search-id (getf e :id)))
                (equal search-id id))) tags)))

(defun get-tag (id)
  "get tag by ID then convert to json"
  (let ((tag (get-tag-by-id id)))
    (encode-plist-to-json-as-string tag)))

(defun get-tag-list ()
  "get tag list and encode as json"
  (encode-multiple-plists-to-json-as-string (fetch-or-create-tags)))

(defun tag-data-get (id)
  "get tag by ID or entire list"
  (if id
      (get-tag (parse-integer id))
      (get-tag-list)))

(define-data-update-handler tag-data-add (model)
    "add tag data to persisted data"
  (let ((new-id (getf model :id))
        (existing-tags (fetch-or-create-tags))
        (user-data-path (get-user-data-path nil :by :login)))
    (write-complete-file (concatenate 'string user-data-path "/" *tag-file-name*) (append existing-tags (list model)))
    (json:encode-json-to-string (list new-id))))

(defun get-tag-todo-by-id (id)
  "get tag todo association by unique ID"
  (let ((tag-todos (test-json-data)))
         (find-if
          #'(lambda (e)
              (let ((search-id (getf e :id)))
                (equal search-id id))) tag-todos)))

(defun get-tag-todo (id)
  "get tag todo association by ID then convert to json"
  (let ((tag-todo (get-tag-todo-by-id id)))
    (encode-plist-to-json-as-string tag-todo)))

(defun get-tag-todo-list ()
  "get tag todo association list and encode as json"
  (encode-multiple-plists-to-json-as-string (fetch-or-create-tag-todos)))

(defun tag-todo-data-get (id)
  "get tag todo association by ID or entire list"
  (if id
      (get-tag-todo (parse-integer id))
      (get-tag-todo-list)))

(defun fetch-or-create-tag-todos (&optional (get-user-data-path #'get-user-data-path) )
  "get tag todo associations from persisted data"
  (let ((user-data-path (if get-user-data-path (funcall get-user-data-path nil :by :login) "")))
    (fetch-or-create-data (concatenate 'string user-data-path "/" *tag-todo-file-name*))))

(define-data-update-handler tag-todo-data-add (model)
    "add tag todo association data to persisted data"
  (let ((new-id (getf model :id))
        (existing-tags (fetch-or-create-tag-todos))
        (user-data-path (get-user-data-path nil :by :login)))
    (write-complete-file (concatenate 'string user-data-path "/" *tag-todo-file-name*) (append existing-tags (list model)))
    (json:encode-json-to-string (list new-id))))


(define-data-update-handler tags-todo-data-add (model)
  "add tag todo associations data to persisted data - use with new todo"
  (flet ((fill-out-tags-todo-list (model)
           (let ((new-id (getf model :todo-id))
                 (tag-ids (getf model :tag-ids)))
             (reduce #'(lambda (acc cur) (append acc (list (list :todo-id new-id :tag-id cur)))) tag-ids :initial-value ()))))
    (let ((existing-tags (fetch-or-create-tag-todos))
          (user-data-path (get-user-data-path nil :by :login)))
      (write-complete-file (concatenate 'string user-data-path "/" *tag-todo-file-name*) (append existing-tags (fill-out-tags-todo-list model)))
      (json:encode-json-to-string (list 'ok)))))))
