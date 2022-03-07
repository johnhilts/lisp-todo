(in-package #:todo-project)

(defun client-recipe ()
  "define client side functions to handle recipes"
  (ps
    (defvar recipe-list ([]))))

(define-for-ps send-new-recipe-item-to-server (recipe-item &optional optional-call-back)
  "save new recipe on server"
  (with-callback
      (send-to-server *recipe-api-endpoint* "POST" recipe-item)
    (optional-call-back)))

(define-for-ps get-recipe-list-from-server (&optional optional-call-back)
  "define callback and get todo list from server and re-render html elements"
  (with-callback
      (get-from-server *recipe-api-endpoint*)
    (let ((server-recipe-list (ps:chain -j-s-o-n (parse (@ this response-text)))))
      (render-recipe-list server-recipe-list)
      (setf recipe-list server-recipe-list)
      (when optional-call-back
        (optional-call-back))))
  t)

(define-for-ps add-recipe (evt)
  "add recipe on client and server and re-render html elements"
  (ps:chain evt (prevent-default))
  (let* ((recipe-name (ps:chain (ps:chain document (get-element-by-id "recipe-name")) value))
         (recipe-ingredients (ps:chain (ps:chain document (get-element-by-id "recipe-ingredients-entry")) value))
         (recipe-steps (ps:chain (ps:chain document (get-element-by-id "recipe-steps-entry")) value)))
    (with-callback
        (get-recipe-list-from-server)
      (let* ((next-id (get-next-index recipe-list))
             (recipe-item  (create name recipe-name ingredients recipe-ingredients steps recipe-steps id next-id)))
        (ps:chain recipe-list (push recipe-item))
        (with-callback
            (send-new-recipe-item-to-server recipe-item)
          (with-callback
              (get-recipe-list-from-server)
            (render-recipe-list))))))
  t)
