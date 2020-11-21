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

(defun fetch-or-create-web-settings ()
  "read web-settings from persistence store; create default if doesn't exist yet"
  (let* ((default-web-settings  (list :web-port 80))
         (call-back #'(lambda (web-settings) (if web-settings web-settings default-web-settings))))
    (fetch-or-create-data *web-settings-file-path* call-back)))

(defparameter *the-http-server* (start-server (getf (fetch-or-create-web-settings) :web-port)))

(defun stop-server (server)
  "Stop the server"
  (stop server))

(defun start-web-app ()
  "start the web app"
  (publish-static-content))

