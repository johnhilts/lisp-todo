(in-package #:todo-project)

(define-api-endpoint todo-data *todo-api-endpoint* (id)
  "REST endpoint for todos"
  (case verb
    (:put
     (todo-data-update raw-data))
    (:post
     (todo-data-add raw-data))
    (:delete
     (todo-data-delete raw-data))
    (:get
     (todo-data-get id))))

(define-api-endpoint tag-data *tag-api-endpoint* (id)
  "REST endpoint for tags"
  (case verb
    (:post
     (tag-data-add raw-data))
    ;; (:delete
    ;;  (todo-data-delete raw-data))
    (:get
     (tag-data-get id))))

(define-api-endpoint tag-todo-data *tag-todo-api-endpoint* (id)
  "REST endpoint for tag todo pairs"
  (case verb
    (:put
     (tag-todo-data-update raw-data))
    (:post
     (tags-todo-data-add raw-data))
    (:delete
     (tag-todo-data-delete raw-data))
    (:get
     (tag-todo-data-get id))))

(define-api-endpoint tag-mru-data *tag-mru-api-endpoint* ()
  "REST endpoint for tag MRUs"
  (case verb
    ;; (:put
    ;;  (tag-mru-data-update raw-data))
    ;; (:post
    ;;  (tags-mru-data-add raw-data))
    (:get
     (get-tag-mru-list))))

(define-api-endpoint app-settings-data *app-settings-api-endpoint* ()
  "REST endpoint for app settings"
  (case verb
    (:put
     (app-settings-data-update raw-data))
    (:get
     (app-settings-data-get))))

(define-api-endpoint recipe-data *recipe-api-endpoint* (id)
  "REST endpoint for recipes"
  (case verb
    (:post
     (recipe-data-add raw-data))
    (:get
     (recipe-data-get id))))
