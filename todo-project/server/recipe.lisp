(in-package #:todo-project)

;; (defun get-recipe-by-id (id)
;;   "get recipe by unique ID"
;;   (let ((recipes (test-json-data)))
;;          (find-if
;;           #'(lambda (e)
;;               (let ((search-id (getf e :id)))
;;                 (equal search-id id))) recipes)))

;; (defun get-recipe (id)
;;   "get recipe by ID then convert to json"
;;   (let ((recipe (get-recipe-by-id id)))
;;     (encode-plist-to-json-as-string recipe)))

(defun fetch-or-create-recipes (&optional (get-user-data-path #'get-user-data-path) )
  "get todo from persisted data"
  (let ((user-data-path (if get-user-data-path (funcall get-user-data-path nil :by :login) "")))
    (fetch-or-create-data (concatenate 'string user-data-path "/"  *recipe-file-name*))))

(defun get-recipe-list ()
  "get todo list and encode as json"
  (encode-multiple-plists-to-json-as-string (fetch-or-create-recipes)))

(defun recipe-data-get (id)
  "get recipe by ID or entire list"
  (if id
      (print (parse-integer id)) ;; this isn't used
      (get-recipe-list)))

(defun parse-recipe-raw-data (model)
  (setf (getf model :ingredients) (split-string-by #\Newline (getf model :ingredients)))
  (setf (getf model :steps) (split-string-by #\Newline (getf model :steps)))
  model)

(define-data-update-handler recipe-data-add (model)
  "add recipe data to persisted data"
  (let ((new-id (getf model :id))
        (existing-recipes (fetch-or-create-recipes))
        (parsed-data (parse-recipe-raw-data model))
        (user-data-path (get-user-data-path nil :by :login)))
    (write-complete-file (concatenate 'string user-data-path "/"  *recipe-file-name*) (append existing-recipes (list parsed-data)))
    (json:encode-json-to-string (list new-id))))
