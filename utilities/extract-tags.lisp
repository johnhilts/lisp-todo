(defpackage #:todo-project-utility (:use #:cl))

(in-package #:todo-project-utility)

(defparameter *nfs-todos* ())

(defparameter *user-file-location*
  "/home/jfh/code/lisp/source/web/todo/todo-project/users/1041626-21860-8277144-83417/"
  ;; "/home/john/nfs-backup-todo-list.sexp"
  )

(defparameter *show-verbose* nil)

(defun extract-tag (string &optional (show-verbose-output *show-verbose*))
  (let ((scanner (cl-ppcre:create-scanner "^(.*?) - ")))
    (multiple-value-bind
	  (s e gs ge)
	(cl-ppcre:scan scanner string)
      (when s
	(when show-verbose-output
	  (format t "start: ~d end: ~d group: ~d - ~d~%" s e gs ge)
	  (format t "~a~%" (subseq string (aref gs 0) (aref ge 0))))
	(subseq string (aref gs 0) (aref ge 0))))))

;; "closet shelf - Box height + width: 11\"3/4 - depth is shorter"

(defun extract-tags (todos)
  (remove-duplicates
   (remove-if-not
    #'identity
    (loop for todo in todos
	 collect (extract-tag (getf todo :text))))
    :test #'string-equal))

(defun create-tag-list (todos)
  (let ((unique-tags (extract-tags todos)))
    (do
     ((index 1 (incf index))
      (tags unique-tags (cdr tags))
      (tag-list ()))
     ((null tags) (reverse tag-list))
      (push (list :id index :text (car tags)) tag-list))))

;; (defun create-tag-todo-pair (todo tag)
;;   (let ((todo-id (getf todo :id))
;; 	(tag-id (getf tag :id)))
;;     (list :todo-id todo-id :tag-id tag-id)))
(defun get-tag-demarker (todo-text)
  (cond
    ((position #\- todo-text)
     #\-)
    ((position #\: todo-text)
     #\:)))

(defun find-todos-that-match-tag (todos tag)
  (let ((tag-id (getf tag :id))
	(tag-text (getf tag :text))
	(results ()))
    (do
     ((search-todos todos (cdr search-todos)))
     ((null search-todos) (reverse results))
      (let* ((todo (car search-todos))
	     (todo-text (getf todo :text))
	     (search-result (search tag-text todo-text :test #'string-equal)))
	(when (and
	       (numberp search-result)
	       (zerop search-result)
	       (get-tag-demarker todo-text))
	  (push (list :todo-id (getf todo :id) :tag-id tag-id) results))))))

(defun create-tag-todo-pairs (todos tags)
  (mapcan
   #'(lambda (tag) (find-todos-that-match-tag todos tag))
   tags))

(defun get-todos-with-tag (todos tag-todo-pairs)
  (let ((todo-ids (mapcar #'(lambda (tag-todo-pair) (getf tag-todo-pair :todo-id)) tag-todo-pairs)))
    (remove-if-not
     #'(lambda (todo)
	 (find (getf todo :id) todo-ids))
     todos)))

(defun update-todo-to-remove-tag-text (todo)
  (let ((todo-text (getf todo :text))) 
    (let* ((demarker-position (position (get-tag-demarker todo-text) todo-text))
           (updated-todo-text (subseq todo-text (+ 2 demarker-position) (length todo-text))))
      (setf (getf todo :text) updated-todo-text)
      todo)))

(defun update-todos-to-remove-tag-text (todos tag-todo-pairs)
  (let ((todos-with-tags (get-todos-with-tag todos tag-todo-pairs)))
    (mapcar
     #'update-todo-to-remove-tag-text
     todos-with-tags)))

(defun get-untagged-todos (todos tag-todo-pairs)
  (let ((todos-with-tags (get-todos-with-tag todos tag-todo-pairs))) 
      (set-difference todos todos-with-tags)))

(defun combine-updated-todos-with-untagged-todos (updated-todos todos tag-todo-pairs)
  (union
   (get-untagged-todos todos tag-todo-pairs)
   updated-todos))

(defun get-updated-todos (todos tag-todo-pairs)
  (let* ((updated-todos (update-todos-to-remove-tag-text todos tag-todo-pairs)))
    (combine-updated-todos-with-untagged-todos updated-todos todos tag-todo-pairs)))

(defun write-complete-file (path list)
  "write complete file all at once"
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (prin1 list out))) ;; print is just like prin1, except it precedes each output with a line break, and ends with a space

(defun get-todo-and-tag-data ()
  (flet ((normalize-single-quotes (todo)
           (setf #1=(getf todo :text) (substitute #\' #\’ #1#))
           todo))
    (let ((nfs-todos ()))
      (with-open-file (in (concatenate 'string *user-file-location* "nfs-backup-todo-list.sexp") :direction :input)
        (push (read in) nfs-todos))
      (let* ((todos (mapcar #'normalize-single-quotes (car nfs-todos)))
             (tags (create-tag-list todos))
             (tag-todo-pairs (create-tag-todo-pairs todos tags)))
        (values todos tags tag-todo-pairs)))))

(defun update-files (todos tags tag-todo-pairs)
  (write-complete-file (concatenate 'string *user-file-location* "/" "todo-list.sexp") (get-updated-todos todos tag-todo-pairs))
  (write-complete-file (concatenate 'string *user-file-location* "/" "tag-todo-list.sexp") tag-todo-pairs)
  (write-complete-file (concatenate 'string *user-file-location* "/" "tag-list.sexp") tags)
  t)

(defun get-tag-ids-with-reference-counts (tag-todo-pairs)
  (let ((tag-reference-table (make-hash-table)))
    (mapc (lambda (tag-todo-pair)
            (let* ((tag-id (getf tag-todo-pair :tag-id))
                   (entry (gethash tag-id tag-reference-table 0)))
              (setf (gethash tag-id tag-reference-table) (incf entry))))
          tag-todo-pairs)
    (let ((tag-reference-list ()))
      (maphash
       (lambda (k v)
         (push (list :tag-id k :reference-count v) tag-reference-list))
       tag-reference-table)
      (sort
       tag-reference-list
       #'(lambda (tag-reference-count1 tag-reference-count2) (>= tag-reference-count1 tag-reference-count2))
       :key #'(lambda (tag-reference) (getf tag-reference :reference-count))))))

(defun get-top-10-tag-ids-by-reference (tag-reference-list)
  (subseq tag-reference-list 0 10))

(defun orchestrator-update-files ()
  (multiple-value-bind (todos tags tag-todo-pairs)
      (get-todo-and-tag-data)
    (update-files todos tags tag-todo-pairs)))

(defun orchestrator-make-tag-mru ()
  (multiple-value-bind (todos tags tag-todo-pairs)
      (get-todo-and-tag-data)
    (declare (ignore todos tags))
    (let* ((complete-mru (get-tag-ids-with-reference-counts tag-todo-pairs))
           ;; (top-10-mru (get-top-10-tag-ids-by-reference complete-mru))
           )
      (write-complete-file (concatenate 'string *user-file-location* "/" "tag-mru-list.sexp") complete-mru)      
      ;; (write-complete-file (concatenate 'string *user-file-location* "/" "tag-top-mru-list.sexp") top-10-mru)      
      (values
       complete-mru
       ;; top-10-mru
       ))))

(defun update-tag-mru (complete-mru top-10-mru tag-id reference-count)
  (flet ((update-complete-mru ()
           (setf (cdr (nth (position tag-id complete-mru :key #'car) complete-mru)) reference-count)
           (sort complete-mru #'>= :key #'cdr))
         (update-top-10-mru ()
           (let ((insert-index (position-if #'(lambda (count) (> count reference-count)) top-10-mru :key #'cdr :from-end t)))
             (if (and
                  insert-index
                  (< insert-index (1- (length top-10-mru))))
                 (append
                  (subseq top-10-mru 0 (1+ insert-index))
                  (list (cons tag-id reference-count))
                  (subseq top-10-mru (1+ insert-index) (- (length top-10-mru) 1)))
                 top-10-mru))))
    (values
     (update-complete-mru)
     (update-top-10-mru))))

#|
Fix these manually!
(((:ID 5 :TEXT "Lights") (:ID 64 :TEXT "Christmas Lights 2021")) ;; "Lights 2022"
 
 ((:ID 15 :TEXT "Sunny") (:ID 19 :TEXT "Shopping / Sunny")) ;; Just "Sunny"
 ((:ID 25 :TEXT "Hellas bakery") (:ID 23 :TEXT "Hellas bakery.")) ;; Remove "."
 ((:ID 41 :TEXT "House") (:ID 18 :TEXT "House Subaru")) ;; Just "Subaru"
 ((:ID 78 :TEXT "Shoe boxes") (:ID 57 :TEXT "Shoe boxes "));; remove trailing space

** ALSO ** Trader Joe's
|#
