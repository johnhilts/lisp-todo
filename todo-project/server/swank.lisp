(in-package #:todo-project)

(defvar *swank-port* nil
  "Port for starting swank. If nil swank won't be started.")

(defmacro when-it (test &body body)
  "Like WHEN. IT is bound to TEST."
  `(let ((it ,test))
     (when it
       ,@body)))

(defun start-swank (&optional port)
  (when-it (or port *swank-port*)
    (let ((*debug-io* (make-broadcast-stream)))
      (swank:create-server :port it
                           :dont-close t)
      (format t "Started swank at port: ~A." it))))

(defun stop-swank (&optional port)
  (when-it (or port *swank-port*)
    (swank:stop-server it)
    (format t "Stopped swank at port: ~A." it)))

