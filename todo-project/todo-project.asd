;;;; todo-project.asd

(asdf:defsystem #:todo-project
  :description "Web App to manage To-Do List"
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-who #:hunchentoot #:parenscript #:cl-json #:jfh-web)
  :components ((:file "package")
               (:file "todo-project")))
