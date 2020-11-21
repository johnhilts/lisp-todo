
(in-package #:todo-project)

(define-api-endpoint todo-data *todo-api-endpoint* (id)
  "REST endpoint for todos"
  (case verb
    (:put
     (todo-data-update raw-data))
    (:post
     (todo-data-add raw-data))
    (:get
     (todo-data-get id))))

(define-api-endpoint app-settings-data *app-settings-api-endpoint* ()
  "REST endpoint for app settings"
  (case verb
    (:put
     (app-settings-data-update raw-data))
    (:get
     (app-settings-data-get))))
