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

(defun fetch-or-create-recipes ()
  "get todo from persisted data"
  (fetch-or-create-data *recipe-file-path*))

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
        (parsed-data (parse-recipe-raw-data model)))
    (write-complete-file *recipe-file-path* (append existing-recipes (list parsed-data)))
    (json:encode-json-to-string (list new-id))))
