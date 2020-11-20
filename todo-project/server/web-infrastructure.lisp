(in-package #:todo-project)

(defun publish-static-content ()
  "static content"
  (push (create-static-file-dispatcher-and-handler
         "/styles.css" "static/styles.css") *dispatch-table*))

(defun start-server (port)
  "start or re-start the web server; this gets called automatically when the server variable is declared"
  (restart-case (start (make-instance 'easy-acceptor :port port))
    (re-start-server ()
      :report "Restart Web Server"
      (stop-server *the-http-server*)
      (start-server port))))

(defparameter *the-http-server* (start-server 5050))

(defun stop-server (server)
  "Stop the server"
  (stop server))

(defun start-web-app ()
  "start the web app"
  (publish-static-content))

