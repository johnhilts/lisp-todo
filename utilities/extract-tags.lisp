(defpackage #:todo-project-utility (:use #:cl))

(in-package #:todo-project-utility)

(defparameter *nfs-todos* ())     

(with-open-file (in "/home/john/nfs-backup-todo-list.sexp" :direction :input)
  (push (read in) *nfs-todos*))

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

(defun extract-tags ()
  (remove-duplicates
   (remove-if-not
    #'identity
    (loop for todo in (car *nfs-todos*)
	 collect (extract-tag (getf todo :text))))
    :test #'string-equal))

(defun create-tag-list ()
  (let ((unique-tags (extract-tags)))
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
    (let ((demarker-position (position (get-tag-demarker todo-text) todo-text)))
      (subseq todo-text (+ 2 demarker-position) (length todo-text)))))

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

(defparameter *todos* (car *nfs-todos*))
(defparameter *tags* (create-tag-list))
(defparameter *tag-todo-pairs* (create-tag-todo-pairs *todos* *tags*))

(let* ((updated-todos (update-todos-to-remove-tag-text *todos* *tag-todo-pairs*)))
  (combine-updated-todos-with-untagged-todos updated-todos *todos* *tag-todo-pairs*))
