
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
