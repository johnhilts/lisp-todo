
(in-package #:todo-project)

(setf (html-mode) :html5)

;; allow parenscript and cl-who to work together
(setf *js-string-delimiter* #\")

(defun make-todo-page ()
  "generate todo HTML page"
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:title "Todo List")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/styles.css")
            (:script :type "text/javascript"
                     (str (setup-client-info))
                     (str (jfh-web:define-ps-with-html-macro))
                     (str (share-server-side-constants))
                     (str (ajax))
                     (str (client-util))
                     (str (client-todo))
                     (str (client-app-settings))
                     (str (client-ui))))
           (:body
            (:div :id "app-settings")
            (:div
             (:h1 "Todo List"
                  (:div
                   (:textarea :id "todo-content" :placeholder "Enter Todo info here." :rows "5" :cols "100")
                   (:button :id "todo-add-btn" "Add"))
                  (:div
                   (:table :id "todo-list"
                           (:thead (:th :id "todo-list-column-header" "To-do Items"))
                           (:tbody :id "todo-list-body" (:tr (:td "(To-do list empty)")))))))))))

(define-easy-handler (todo-page :uri "/todos") ()
  "HTTP endpoint"
  (make-todo-page))
