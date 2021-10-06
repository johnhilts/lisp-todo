
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

(defun get-next-todo-index (todo-list)
  "calculate next index for todo list"
  (let ((id-list (mapcar #'(lambda (todo) (getf todo :id)) todo-list)))
    (if (null id-list)
        1
        (+ 1 (apply #'max id-list)))))

(defun transform-lines-to-todos (lines start-new-id formatted-list-name)
  "transform lines (string ending in #\Newline) into a list of todo items"
  (let ((split-lines (split-string-by #\Newline lines)))
    (reduce #'(lambda (acc cur)
                (let* ((prefix formatted-list-name)
                       (text (format nil "~a~a" prefix cur))
                       (todo (list (list :text text :done 0 :id (+ start-new-id (length acc))))))
                  (append acc todo)))
            split-lines :initial-value nil)))

(defun import-lines-into-todo-list (lines list-name)
  "orchestrator called by web handler to take input and output it in desired form"
  (flet ((get-list-name (input-list-name new-id)
           (let ((list-name (or input-list-name "")))
             (if (plusp (length list-name)) (format nil "~a - " list-name) (format nil "List ~d - " new-id)))))
    (let* ((existing-todos (fetch-or-create-todos))
           (new-id (get-next-todo-index existing-todos))
           (formatted-list-name (get-list-name list-name new-id)))
      (write-complete-file *todo-file-path* (append existing-todos (transform-lines-to-todos lines new-id formatted-list-name)))
      (let ((app-settings (fetch-or-create-app-settings)))
        (setf (getf app-settings :filter-text) formatted-list-name)
        (write-complete-file *app-settings-file-path* app-settings))))
  t)

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

(define-data-update-handler todo-data-delete (model)
  "delete todo by ID"
  (let ((delete-id (getf model :id)))
    (when delete-id
      (let* ((existing-todos (fetch-or-create-todos))
             (deleted-item-position (position-if #'(lambda (e) (= (getf e :id) delete-id)) existing-todos))
             (updated-todos (splice-and-remove-item-in-list existing-todos deleted-item-position)))
        (write-complete-file *todo-file-path* updated-todos)
        (json:encode-json-to-string (list delete-id))))))

