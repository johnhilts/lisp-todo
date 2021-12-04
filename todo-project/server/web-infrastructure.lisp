(in-package #:todo-project)

(defun publish-static-content ()
  "static content"
  (push (create-static-file-dispatcher-and-handler
         "/favicon.ico" "ez-favicon.ico") *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
         "/styles.css" "static/styles.css") *dispatch-table*))

(defparameter *the-http-server* nil)

(defun stop-server (server)
  "Stop the server"
  (stop server))

(defun start-server (port)
  "start or re-start the web server; this gets called automatically when the server variable is declared"
  (setf *the-http-server*
        (restart-case (start (make-instance 'easy-ssl-acceptor :port port :ssl-privatekey-file #P"../certs/server.key" :ssl-certificate-file #P"../certs/server.crt"))
          (re-start-server ()
            :report "Restart Web Server"
            (stop-server *the-http-server*)
            (start-server port)))))

(defun fetch-or-create-web-settings ()
  "read web-settings from persistence store; create default if doesn't exist yet"
  (let* ((default-web-settings  (list :web-port 80))
         (call-back #'(lambda (web-settings) (if web-settings web-settings default-web-settings))))
    (fetch-or-create-data *web-settings-file-path* call-back)))

(defun start-web-app ()
  "start the web app"
  (start-server (getf (fetch-or-create-web-settings) :web-port))
  (setf *session-max-time* (* 24 3 60))
  (setf *rewrite-for-session-urls* nil)
  (publish-static-content)
  (let ((user-index-path (format nil "~a/user-index.sexp" *users-root-folder-path*)))
     (ensure-directories-exist user-index-path)))

(defun stop-web-app ()
  "stop the web app"
  (stop-server *the-http-server*))

