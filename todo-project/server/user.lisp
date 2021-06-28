
(in-package #:todo-project)

(defun hydrate-user-info (name login password)
  (let ((user (make-instance (define-info-class user name login password))))
    (populate-info-object user name login password)))

(defun read-user-index ()
  (let ((user-index (with-open-file (in (format nil "~a/user-index.sexp" *users-root-folder-path*))
                      (read-line in))))
    (do ((file-index 0)
         (list)
         (eof))
        (eof list)
      (multiple-value-bind (item item-length)
          (read-from-string (subseq user-index file-index))
        (push item list)
        (setf file-index (+ file-index item-length))
        (setf eof (>= file-index (length user-index)))))))

(defun read-user-info (guid)
  "read user-info from guid/user.sexp The guid is needed to find thefolder."
  (let ((user-entry (read-complete-file (format nil "~a/~a/user.sexp" *users-root-folder-path* guid))))
    (let ((name (car user-entry))
          (login (cadr user-entry))
          (password (caddr user-entry)))
      (hydrate-user-info name login password))))

(defun add-user (name login password)
  (flet ((add-user-to-index (login user-guid)
           (let ((user-index-path (format nil "~a/user-index.sexp" *users-root-folder-path*)))
             (append-to-file (ensure-directories-exist user-index-path) (list login user-guid)))))
  (let* ((user-guid (generate-unique-token))
         (user-path (format nil "~a/~a/user.sexp" *users-root-folder-path* user-guid)))
    (write-complete-file (ensure-directories-exist user-path) (info-object-to-list user name login password))
    (add-user-to-index login user-guid)
    (setf *user-index* (read-user-index)))))

(defun find-user-index-entry (search-value &key by)
  (let ((user-index (or *user-index* (read-user-index))))
    (case by
      (:login (find search-value user-index :test #'(lambda (search-value e) (string= search-value (car e)))))
      (:guid (find search-value user-index :test #'(lambda (search-value e) (string= search-value (cadr e))))))))

(defun find-user-entry (search-value &key by)
  (ecase by
    (:login
     (let* ((user-index-entry (find-user-index-entry search-value :by by))
            (user-guid (cadr user-index-entry)))
       (read-user-info user-guid)))
    (:guid (read-user-info search-value))))
