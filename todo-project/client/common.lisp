
(in-package #:todo-project)

(defmacro defun-for-ps (name args &body body)
  `(defun ,name ()
     (ps
       (defun ,name (,@args)
         ,@body))))

(defun-for-ps my-test-function (arg1 arg2)
  (let ((result (create field1 arg1 field2 arg2)))
    result))
