
(in-package #:todo-project)

(defmacro define-api-endpoint (name end-point params &body body)
  `(define-easy-handler (,name :uri ,end-point) (,@params)
     "macro to DRY REST endpoint declarations"
     (setf (content-type*) "application/json")
     (let* ((raw-data  (raw-post-data :force-text t))
            (verb (request-method *request*)))
       ,@body)))

(defmacro define-data-update-handler (name model &body body)
  (let ((model-name (car model)))
    `(defun ,name (raw-data)
       (let ((,model-name (convert-dotted-pair-to-plist (json:decode-json-from-string raw-data))))
         ,@body))))

(defmacro define-info-class (name &rest slots)
  (labels ((make-slot (name slot)
             (read-from-string
              (concatenate 'string (string name) "-" (string slot)))))
    (flet ((make-slot-list-item (slot)
             (list slot :accessor (make-slot name slot)))
           (make-name (name)
             (read-from-string
              (concatenate 'string (string name) "-info"))))      
      (let* ((class-name (make-name name))
             (class-slots (mapcar #'make-slot-list-item slots)))
        `(defclass ,class-name ()
           ,class-slots)))))

(defmacro populate-info-object (name &rest slots)
  (flet ((get-setter (slot)
           (let ((expression (read-from-string (concatenate 'string "((" (string name) "-" (string slot) " " (string name) ") " (string slot) ")"))))
             (car `((setf ,(car expression) ,(cadr expression)))))))
    (let ((object-name  (read-from-string (string name)))
          (setters (mapcar #'get-setter slots)))
      `(progn
         ,@setters
         ,object-name))))

(defmacro object-to-list (name &rest slots)
  (flet ((access-slots (slot)
           (read-from-string (format nil "(~a-~a ~a-info)" name slot name))))
    (let* ((hydrate-name (read-from-string (format nil "hydrate-~a-info" name)))
           (object-name (read-from-string (format nil "~a-info" name)))
           (access-slots (mapcar #'access-slots slots)))
      `(let ((,object-name (,hydrate-name ,@slots)))
         (list ,@access-slots)))))

(defmethod get-parsed-date ((date date-info))
  (multiple-value-bind
        (second minute hour day month year day-of-the-week daylight-p zone)
      (decode-universal-time (get-universal-time))
    (populate-info-object date second minute hour day month year day-of-the-week daylight-p zone)))
