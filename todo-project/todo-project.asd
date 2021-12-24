;;;; todo-project.asd
;;;; (asdf:load-system "todo-project")
;;;; see load-dependencies-to-asdf.lisp for how to load all local dependencies

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(defun read-complete-file (path)
  "read complete file all at once"
  (with-open-file (in path :if-does-not-exist :create)
    (read in nil)))

;; "home/public/source/lib/lisp-util-lib/web/jfh-web/jfh-web.lisp"
(let* ((app-root (cdr (assoc :app-root (read-complete-file ".config")))) 
       (lib-root (cdr (assoc :lib-root (read-complete-file ".config"))))
       (jfh-web-lib-path (make-pathname :directory (concatenate 'string lib-root "/web/jfh-web")))
       (jfh-web-source-path (probe-file (concatenate 'string "/" lib-root "/web/jfh-web/jfh-web.lisp")))
       (todo-app-path (make-pathname :directory app-root)))
  
  (push jfh-web-lib-path asdf:*central-registry*)
;;; (push #p"/home/jfh/code/lisp/source/util-lib/testing/jfh-testing/" asdf:*central-registry*)
;;; (push #p"/home/jfh/code/lisp/source/util-lib/web/test/jfh-web-test/" asdf:*central-registry*)
  (push todo-app-path asdf:*central-registry*)
  (asdf:load-system "jfh-web")
  (compile-file jfh-web-source-path))
;;; (asdf:load-system "jfh-testing")
;;; (asdf:load-system "jfh-web-test")
;;; (asdf:load-system "todo-project")

(asdf:defsystem #:todo-project
  :description "Web App to manage To-Do List"
  :author "John Hilts <johnhilts@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-who #:hunchentoot #:parenscript #:cl-json #:jfh-web #:swank #:ironclad)
  :components ((:file "package")
               (:file "common/constants")
               (:file "macros")
               (:file "server/swank")
               (:file "server/web-common")
               (:file "server/util")
               (:file "server/io")
               (:file "server/system-settings")
               (:file "server/web-infrastructure")
               (:file "server/app-settings")
               (:file "server/user")
               (:file "server/todo")
               (:file "server/recipe")
               (:file "server/api")
               (:file "client/common")
               (:file "client/ajax")
               (:file "client/util")
               (:file "client/app-settings")
               (:file "client/todo")
               (:file "client/recipe")
               (:file "client/ui")
               (:file "server/html")
               (:file "main")))

(defun buildapp ()
  (asdf:load-system :todo-project)
  (save-lisp-and-die "todo-app"
                     :toplevel 'cl-user::main
                     :executable t))
