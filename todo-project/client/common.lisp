
(in-package #:todo-project)

(defparameter *registered-ps-functions* ())

(defmacro define-for-ps (name args &body body)
  (list 'progn
        `(defun ,name ()
           (ps
             (defun ,name (,@args)
               ,@body)))
        `(unless (member ',name *registered-ps-functions*)
           (setf (getf *registered-ps-functions* ',name) #',name))))
