(in-package #:todo-project)

(defun share-server-side-constants ()
  "feed server side constants to parenscript"
  (ps:ps
    (defmacro share-server-side-constants ()
      (flet (
             (a-defvar (e) (equal 'defvar (car e)))
             (constants-from-server ()
               (read-complete-file-by-line "./common/constants.lisp")))
        `(progn
           ,@(mapcar #'print
              (remove-if-not #'a-defvar
                             (constants-from-server))))))

    (share-server-side-constants)))

(define-for-ps get-next-index (list-with-id)
  "calculate next index for any list with an id field"
  (let ((id-list (chain list-with-id (map #'(lambda (item) (@ item id)))))
        (max-fn (@ -Math max)))
    (if (length id-list)
        (+ 1 (ps:chain max-fn (apply null id-list)))
        1)))
