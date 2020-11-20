
(in-package #:todo-project)

(defmacro define-api-endpoint (name end-point params &body body)
  `(define-easy-handler (,name :uri ,end-point) (,@params)
     "macro to DRY REST endpoint declarations"
     (setf (content-type*) "application/json")
     (let* ((raw-data  (raw-post-data :force-text t))
            (verb (request-method *request*)))
       ,@body)))

(define-api-endpoint todo-data "/todo-data" (id)
  (case verb
    (:put
     (todo-data-update raw-data))
    (:post
     (todo-data-add raw-data))
    (:get
     (todo-data-get id))))

(define-api-endpoint app-settings-data "/app-settings-data" ()
  (case verb
    (:put
     (app-settings-data-update raw-data))
    (:get
     (app-settings-data-get))))
