
(in-package #:todo-project)

(defparameter *registered-ps-functions* ())

(defmacro define-for-ps (name args &body body)
  (list 'progn
        `(defun ,name ()
           (ps
             (defun ,name (,@args)
               ,@body)))
        `(let ((function (member ',name *registered-ps-functions*)))
           (when function
             (remove (car (member ',name *registered-ps-functions*)) (remove (cadr (member ',name *registered-ps-functions*)) *registered-ps-functions*)))
           (setf (getf *registered-ps-functions* ',name) #',name))))
