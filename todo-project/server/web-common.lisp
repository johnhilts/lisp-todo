
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

(defun define-info-classes ()
  "wrapper so that I can easily find where I'm defining my info classes"
  (define-info-class date second minute hour day month year day-of-the-week daylight-p zone)
  (define-info-class user name login password))

((lambda ()
   (define-info-classes)))

(defmacro populate-info-object (name &rest slots)
  (flet ((get-setter (slot)
           (let ((expression (read-from-string (concatenate 'string "((" (string name) "-" (string slot) " " (string name) ") " (string slot) ")"))))
             (car `((setf ,(car expression) ,(cadr expression)))))))
    (let ((object-name  (read-from-string (string name)))
          (setters (mapcar #'get-setter slots)))
      `(progn
         ,@setters
         ,object-name))))

(defmacro info-object-to-list (name &rest slots)
  "given an info object's name and the name of its slots, convert the info object into a list."
  (flet ((access-slots (slot)
           (read-from-string (format nil "(~a-~a ~a-info)" name slot name))))
    (let* ((hydrate-name (read-from-string (format nil "hydrate-~a-info" name)))
           (object-name (read-from-string (format nil "~a-info" name)))
           (access-slots (mapcar #'access-slots slots)))
      `(let ((,object-name (,hydrate-name ,@slots)))
         (list ,@access-slots)))))

(defmacro list-to-info-object-no-let (list name &rest slots)
  "Given a list, an info object's name, and the name of its slots, convert the list into an info object."
  (labels  ((access-slots (slots index)
              (cond
                ((null slots) nil)
                (t (cons `(,(car slots) `(identity ,(nth ,index ,list))) (access-slots (cdr slots) (1+ index)))))))
    (let ((let-var-form (access-slots slots 0))
          (hydrate-form (read-from-string (format nil "(hydrate-~a-info ~{~a ~})" name slots))))
      `(let ,let-var-form
         ,hydrate-form))))

(defmacro list-to-info-object (list name &rest slots)
  "Given a list, an info object's name, and the name of its slots, convert the list into an info object."
  (let ((list-var (gensym))
        (slots-var (gensym)))
    `(let ((,list-var ,list)
           (,slots-var '(,@slots)))
       (labels ((access-slots (slots index)
                  (cond
                    ((null slots) nil)
                    (t (cons `(,(car slots) ,(nth index ,list-var)) (access-slots (cdr slots) (1+ index)))))))
         `(let (,@(access-slots ,slots-var 0))
                                        ; (hydrate-user-info name login password)
            ,'(hydrate-user-info ,@slots))))))

(defmethod get-parsed-date ((date date-info))
  (multiple-value-bind
        (second minute hour day month year day-of-the-week daylight-p zone)
      (decode-universal-time (get-universal-time))
    (populate-info-object date second minute hour day month year day-of-the-week daylight-p zone)))

(defmacro define-protected-page (name-and-end-point params &body body)
  "macro to DRY pages requiring authentication"
  (let* ((name (car name-and-end-point))
        (end-point (cadr name-and-end-point))
        (possible-description (car body))
        (has-description (and (atom possible-description) (stringp possible-description)))
        (description (if has-description possible-description nil))
        (body-after-description (if has-description (cdr body) body)))
    `(define-easy-handler (,name :uri ,end-point) (,@params)
       ,(when description description)
        (multiple-value-bind (authenticated-user present-p)
           (get-authenticated-user)
         (if present-p
             ,@body-after-description
             (redirect (format nil "/login?redirect-back-to=~a" (url-encode ,end-point))))))))

