(in-package #:todo-project)

(defun ajax ()
  (ps

    (defun send-to-server (the-url http-method data)
      "Do AJAX request using PUSH or PUT"
      (let ((xml-http (new (-x-m-l-http-request))))
        (chain xml-http (open http-method the-url))
        (chain xml-http (set-request-header "Content-Type" "application/json;charset=UTF-8"))
        (chain xml-http (send (chain -j-s-o-n (stringify data)))))
      t)

    
    (defun get-settings-from-server ()
      (flet ((req-listener ()
               (let ((server-app-settings (chain -j-s-o-n (parse (@ this response-text)))))
                 (setf *app-settings* server-app-settings)
                 (render-app-settings)
                 t)))
        (let ((o-req (new (-x-m-l-http-request))))
          (chain o-req (add-event-listener "load" req-listener))
          (chain o-req (open "GET" "/setting-data"))
          (chain o-req (send)))))

    (defun get-todo-list-from-server ()
      (flet ((req-listener ()
               (let ((server-todo-list (chain -j-s-o-n (parse (@ this response-text)))))
                 (render-todo-list server-todo-list)
                 (setf todo-list server-todo-list)
                 t)))
        (let ((o-req (new (-x-m-l-http-request))))
          (chain o-req (add-event-listener "load" req-listener))
          (chain o-req (open "GET" "/todo-data"))
          (chain o-req (send)))))))
