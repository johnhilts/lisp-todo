(in-package #:todo-project)

(ps:ps
  (defmacro with-callback (fn &body body)
    `(,(car fn) ,@(cdr fn) #'(lambda (),@body))))

(define-for-ps send-new-tag-item-to-server (tag-item)
  "save new tag on server"
  (send-to-server *tag-api-endpoint* "POST" tag-item))

(define-for-ps get-tag-list-from-server (&optional optional-call-back)
  "define callback and get tag list from server"
  (with-callback
      (get-from-server *tag-api-endpoint*)
    (let ((server-tag-list (ps:chain -j-s-o-n (parse (@ this response-text)))))
      (tag-items 'initialize-tags server-tag-list)
      (when optional-call-back
        (optional-call-back))))
  t)

(define-for-ps add-tag (id-prefix event)
  "add tag on client and server and re-render html elements"
  (ps:chain event (prevent-default))
  (let* ((tag (ps:chain document (get-element-by-id (+ id-prefix "tag-input"))))
         (tag-text (ps:chain tag value)))
    (with-callback
        (get-tag-list-from-server)
      (let* ((next-id (get-next-index (get-all-tags)))
             (tag-item  (ps:create text tag-text id next-id)))
        (tag-items 'add-tag tag-item)
        (ps:chain *selected-tag-ids* (push next-id))
        (render-tag-filter)
        (render-selected-tags *selected-tag-ids* id-prefix)
        (send-new-tag-item-to-server tag-item))))
  t)

(define-for-ps send-new-tag-todo-item-to-server (tag-todo-item)
  "save new tag todo association on server"
  (send-to-server *tag-todo-api-endpoint* "PUT" tag-todo-item))

(define-for-ps send-new-tags-todo-item-to-server (tag-todos)
  "save new tag todo association list on server - use with new todo"
  (send-to-server *tag-todo-api-endpoint* "POST" tag-todos))

(define-for-ps send-updated-tags-todo-item-to-server (tag-todos)
  "save updated tag todo association list on server - use with existing todo"
  (send-to-server *tag-todo-api-endpoint* "PUT" tag-todos))

(define-for-ps delete-tag-todo-item-on-server (delete-id-object call-back)
  "delete tag todo association item on server"
  (send-to-server *tag-todo-api-endpoint* "DELETE" delete-id-object call-back))

(define-for-ps get-tag-todo-associaton-list-from-server (&optional optional-call-back)
  "define callback and get tag todo association list from server"
  (with-callback
      (get-from-server *tag-todo-api-endpoint*)
    (let ((server-tag-todo-list (ps:chain -j-s-o-n (parse (@ this response-text)))))
      (tag-todo-items 'initialize-tag-todos server-tag-todo-list)
      (when optional-call-back
        (optional-call-back))))
  t)

(define-for-ps add-associate-tags-to-todo (todo-id tag-ids)
  "Idempotent function to add associations for tags to a todo - use when adding a new todo"
  (ps:chain event (prevent-default))
  (ps:chain tag-ids (for-each #'(lambda (tag-id) (tag-todo-items 'add-tag-todo (create todo-id todo-id tag-id tag-id)))))
  (send-new-tags-todo-item-to-server (ps:create todo-id todo-id tag-ids tag-ids))
  t)

(define-for-ps edit-associate-tags-to-todo (todo-id tag-ids)
  "Idempotent function to add associations for tags to a todo - use when updating a todo"
  (ps:chain event (prevent-default))
  (tag-todo-items 'update-tags-by-todo-id todo-id tag-ids)
  (send-updated-tags-todo-item-to-server (ps:create todo-id todo-id tag-ids tag-ids))
  t)

(define-for-ps add-associate-tag-to-todo (tag-todo-item)
  "Idempotent function to add an association for a tag to a todo"
  (ps:chain event (prevent-default))
  (let ((tag-todos (get-all-tag-todos)))
    (with-callback
        (get-tag-todo-associaton-list-from-server)
      (with-slots (tag-id todo-id) tag-todo-item
        (let* ((tags-for-current-todo-id (remove-if-not* #'(lambda (tag-todo) (= todo-id (ps:@ tag-todo todo-id))) tag-todos))
               (is-selected-tag #'(lambda (selected-tag-id) (= selected-tag-id (ps:@ tag-todo tag-id))))
               (missing-tags (remove-if-not* #'(lambda (tag-todo) (< (position-if* is-selected-tag *selected-tag-ids*) 0)) tags-for-current-todo-id)))
          (ps:chain missing-tags (for-each #'(lambda (missing-tag) (ps:chain *selected-tag-ids* (push (ps:@ missing-tag tag-id))))))
          (render-selected-tags *selected-tag-ids*))
        (let ((tag-todo-association-exists #'(lambda (e) (and (= tag-id (ps:@ e tag-id)) (= todo-id (ps:@ e todo-id))))))
          (unless (some* tag-todo-association-exists tag-todos) ;; why did we have to add this guard clause?
            (tag-todo-items 'add-tag-todo tag-todo-item)
            (send-new-tag-todo-item-to-server tag-todo-item))))))
  t)

(define-for-ps delete-tag-todo (tag-id todo-id)
  "delete tag todo association on client and server"
  (delete-tag-todo-item-on-server (ps:create tag-id tag-id todo-id todo-id))
  ;; (let ((delete-item-index (ps:chain todo-list (find-index #'(lambda (todo) (= (@ todo id) delete-id))))))
  ;;   (ps:chain todo-list (splice delete-item-index 1)))
  ;; (render-todo-list todo-list)
  t)
