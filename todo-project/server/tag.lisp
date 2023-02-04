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

(defun import-todo-tags-list (tag-id-csv todo-ids)
  "save association of todos and tags that were imported together."
  (flet ((fill-out-tags-todo-list (tag-ids todo-ids)
           (mapcan
            #'(lambda (todo-id)
                (mapcar
                 #'(lambda (tag-id)
                     (list :todo-id todo-id :tag-id tag-id))
                 tag-ids))
            todo-ids)))
    (let ((existing-tags (fetch-or-create-tag-todos))
          (user-data-path (get-user-data-path nil :by :login))
          (tag-ids (mapcar #'parse-integer (split-string-by #\, tag-id-csv))))
      (write-complete-file (concatenate 'string user-data-path "/" *tag-todo-file-name*) (append existing-tags (fill-out-tags-todo-list tag-ids todo-ids)))
      (json:encode-json-to-string (list 'ok)))))

(define-data-update-handler tag-todo-data-update (model)
  "update tag todo association data to persisted data"
  (flet ((fill-out-tags-todo-list (model)
           (let ((todo-id (getf model :todo-id))
                 (tag-ids (getf model :tag-ids)))
             (reduce #'(lambda (acc cur) (append acc (list (list :todo-id todo-id :tag-id cur)))) tag-ids :initial-value ()))))
    (let* ((todo-id (getf model :todo-id)) ;; getting this twice!
           (existing-tags (fetch-or-create-tag-todos))
           (filtered-tags (remove-if #'(lambda (e) (= todo-id (getf e :todo-id))) existing-tags))
           (user-data-path (get-user-data-path nil :by :login)))
      (write-complete-file (concatenate 'string user-data-path "/" *tag-todo-file-name*) (append filtered-tags (fill-out-tags-todo-list model)))
      (json:encode-json-to-string (list todo-id)))))

(define-data-update-handler tags-todo-data-add (model)
  "add tag todo associations data to persisted data - use with new todo"
  (flet ((fill-out-tags-todo-list (model)
           (let ((new-id (getf model :todo-id))
                 (tag-ids (getf model :tag-ids)))
             (reduce #'(lambda (acc cur) (append acc (list (list :todo-id new-id :tag-id cur)))) tag-ids :initial-value ()))))
    (let ((existing-tags (fetch-or-create-tag-todos))
          (user-data-path (get-user-data-path nil :by :login)))
      (write-complete-file (concatenate 'string user-data-path "/" *tag-todo-file-name*) (append existing-tags (fill-out-tags-todo-list model)))
      (json:encode-json-to-string (list 'ok)))))

(define-data-update-handler tag-todo-data-delete (model)
  "delete tag todo association data from persisted data"
  (let* ((todo-id (getf model :todo-id)) ;; getting this twice!
         (tag-id  (getf model :tag-id))
         (existing-tags (fetch-or-create-tag-todos))
         (deleted-item-position (position-if #'(lambda (e) (and (= (getf e :tag-id) tag-id) (= (getf e :todo-id) todo-id))) existing-tags))
         (updated-tags (splice-and-remove-item-in-list existing-tags deleted-item-position))
         (user-data-path (get-user-data-path nil :by :login)))
    (write-complete-file (concatenate 'string user-data-path "/" *tag-todo-file-name*) updated-tags)
    (json:encode-json-to-string (list tag-id todo-id))))

;;;; *************

(defun get-tag-mru-list ()
  "get tag MRU list and encode as json"
  (encode-multiple-plists-to-json-as-string (fetch-or-create-tag-mrus)))

(defun fetch-or-create-tag-mrus (&optional (get-user-data-path #'get-user-data-path) )
  "get tag MRU from persisted data"
  (let ((user-data-path (if get-user-data-path (funcall get-user-data-path nil :by :login) "")))
    (fetch-or-create-data (concatenate 'string user-data-path "/" *tag-mru-file-name*))))

(define-data-update-handler tag-mru-data-update (model)
  "update tag todo association data to persisted data"
  (flet ((fill-out-tags-todo-list (model)
           (let ((todo-id (getf model :todo-id))
                 (tag-ids (getf model :tag-ids)))
             (reduce #'(lambda (acc cur) (append acc (list (list :todo-id todo-id :tag-id cur)))) tag-ids :initial-value ()))))
    (let* ((todo-id (getf model :todo-id)) ;; getting this twice!
           (existing-tags (fetch-or-create-tag-mrus))
           (filtered-tags (remove-if #'(lambda (e) (= todo-id (getf e :todo-id))) existing-tags))
           (user-data-path (get-user-data-path nil :by :login)))
      (write-complete-file (concatenate 'string user-data-path "/" *tag-mru-file-name*) (append filtered-tags (fill-out-tags-todo-list model)))
      (json:encode-json-to-string (list todo-id)))))

(define-data-update-handler tags-todo-data-add (model)
  "add tag todo associations data to persisted data - use with new todo"
  (flet ((fill-out-tags-todo-list (model)
           (let ((new-id (getf model :todo-id))
                 (tag-ids (getf model :tag-ids)))
             (reduce #'(lambda (acc cur) (append acc (list (list :todo-id new-id :tag-id cur)))) tag-ids :initial-value ()))))
    (let ((existing-tags (fetch-or-create-tag-mrus))
          (user-data-path (get-user-data-path nil :by :login)))
      (write-complete-file (concatenate 'string user-data-path "/" *tag-mru-file-name*) (append existing-tags (fill-out-tags-todo-list model)))
      (json:encode-json-to-string (list 'ok)))))

(define-data-update-handler tag-mru-data-delete (model)
  "delete tag todo association data from persisted data"
  (let* ((todo-id (getf model :todo-id)) ;; getting this twice!
         (tag-id  (getf model :tag-id))
         (existing-tags (fetch-or-create-tag-mrus))
         (deleted-item-position (position-if #'(lambda (e) (and (= (getf e :tag-id) tag-id) (= (getf e :todo-id) todo-id))) existing-tags))
         (updated-tags (splice-and-remove-item-in-list existing-tags deleted-item-position))
         (user-data-path (get-user-data-path nil :by :login)))
    (write-complete-file (concatenate 'string user-data-path "/" *tag-mru-file-name*) updated-tags)
    (json:encode-json-to-string (list tag-id todo-id))))
