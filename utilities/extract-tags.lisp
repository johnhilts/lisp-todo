(defpackage #:hacking (:use #:cl)) ;; change to todo-utility

(in-package #:hacking)

(defparameter *nfs-todos* ())     

(with-open-file (in "/home/john/nfs-backup-todo-list.sexp" :direction :input)
  (push (read in) *nfs-todos*))

(defun extract-tag (string)
  (let ((scanner (cl-ppcre:create-scanner "^(.*?) - ")))
    (multiple-value-bind
	  (s e gs ge)
	(cl-ppcre:scan scanner string)
      (when s
	(format t "start: ~d end: ~d group: ~d - ~d~%" s e gs ge)
	(format t "~a~%" (subseq string (aref gs 0) (aref ge 0)))
	(subseq string (aref gs 0) (aref ge 0))))))

;; "closet shelf - Box height + width: 11\"3/4 - depth is shorter"

(defun extract-tags ()
  (remove-duplicates
   (loop for todo in (car *nfs-todos*)
	 collect (extract-tag (getf todo :text)))
    :test #'string-equal))
