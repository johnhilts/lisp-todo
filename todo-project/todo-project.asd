;;;; todo-project.asd
;;;; (asdf:load-system "todo-project")
;;;; see load-dependencies-to-asdf.lisp for how to load all local dependencies

(asdf:defsystem #:todo-project
  :description "Web App to manage To-Do List"
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-who #:hunchentoot #:parenscript #:cl-json #:jfh-web)
  :components ((:file "package")
               (:file "server/util")
               (:file "server/io")
               (:file "server/web-infrastructure")
               (:file "server/app-settings")
               (:file "server/todo")
               (:file "server/html")
               (:file "server/api")
               (:file "client/ajax")
               (:file "client/util")
               (:file "client/app-settings")
               (:file "client/todo")
               (:file "client/ui")
               (:file "main")))
