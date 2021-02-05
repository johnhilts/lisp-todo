;;;; web app entry point

(in-package #:todo-project)

(import-macros-from-lisp 'with-html-elements)


(in-package #:cl-user)

(defun main ()
  (todo-project::start-web-app)
  (sb-impl::toplevel-repl nil))

