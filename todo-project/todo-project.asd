;;;; todo-project.asd
;;;; (asdf:load-system "todo-project")
;;;; see load-dependencies-to-asdf.lisp for how to load all local dependencies

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(push #p"/home/jfh/code/lisp/source/util-lib/web/jfh-web/" asdf:*central-registry*)
;;; (push #p"/home/jfh/code/lisp/source/util-lib/testing/jfh-testing/" asdf:*central-registry*)
;;; (push #p"/home/jfh/code/lisp/source/util-lib/web/test/jfh-web-test/" asdf:*central-registry*)
(push #p"/home/jfh/code/lisp/source/web/todo/todo-project/" asdf:*central-registry*)
(asdf:load-system "jfh-web")
(compile-file #p"/home/jfh/code/lisp/source/util-lib/web/jfh-web/jfh-web.lisp")
;;; (asdf:load-system "jfh-testing")
;;; (asdf:load-system "jfh-web-test")
;;; (asdf:load-system "todo-project")

(asdf:defsystem #:todo-project
  :description "Web App to manage To-Do List"
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-who #:hunchentoot #:parenscript #:cl-json #:jfh-web)
  :components ((:file "package")
               (:file "common/constants")
               (:file "server/web-common")
               (:file "server/util")
               (:file "server/io")
               (:file "server/web-infrastructure")
               (:file "server/app-settings")
               (:file "server/todo")
               (:file "server/api")
               (:file "client/common")
               (:file "client/ajax")
               (:file "client/util")
               (:file "client/app-settings")
               (:file "client/todo")
               (:file "client/ui")
               (:file "server/html")
               (:file "main")))

(defun buildapp ()
  (asdf:load-system :todo-project)
  (save-lisp-and-die "todo-app"
                     :toplevel 'cl-user::main
                     :executable t))
