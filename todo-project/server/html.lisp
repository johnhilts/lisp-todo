
(in-package #:todo-project)

(setf (html-mode) :html5)

;; allow parenscript and cl-who to work together
(setf *js-string-delimiter* #\")

(defun make-todo-page ()
  "generate todo HTML page"
  ;; I split this calling of ps functions into 2 operations because str is a macrolet that's only available under with-html-output-to-string
  ;; I have options to consider such as I can mimic str and then put it whereever I want and then only have to work with 1 list
  (flet ((invoke-registered-ps-functions ()
           "pull all the registered ps functions from a global plist, then put them into a list"
           (do ((e *registered-ps-functions* (cddr e))
                (result ()))
               ((null e) result)
             (push (getf *registered-ps-functions* (car e)) result))))
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
                       (str (jfh-web:define-ps-with-html-macro))
                       (str (share-server-side-constants))
                       (str (client-todo))
                       (str (client-app-settings))
                       (str (client-ui))
                       (dolist (e (invoke-registered-ps-functions))
                         (str (funcall e)))))
             (:body
              (:div :id "app-settings")
              (:div :id "todo-filter")
              (:div
               (:h1 "Todo List"
                    (:div
                     (:textarea :id "todo-content" :placeholder "Enter Todo info here." :rows "5" :cols "100")
                     (:button :id "todo-add-btn" "Add"))
                    (:div
                     (:table :id "todo-list"
                             (:thead (:th :id "todo-list-column-header" "To-do Items"))
                             (:tbody :id "todo-list-body" (:tr (:td "(To-do list empty)"))))))))))))

(define-easy-handler (todo-page :uri "/todos") ()
  "HTTP endpoint for todo list"
  (make-todo-page))

(define-easy-handler (login-page :uri "/login") ()
  "HTTP endpoint for logging in"
  (multiple-value-bind (user password)
      (authorization)
    (cond ((and (equal user "nanook")
                (equal password "igloo"))
           (with-html-output-to-string
               (*standard-output* nil :prologue t :indent t)
             (:html
              (:head (:title "Hunchentoot page with Basic Authentication"))
              (:body
               (:h2 "Hunchentoot page with Basic Authentication")
               (:div "You're logged in!")))))
          (t
           (require-authorization)))))

(define-easy-handler (version-page :uri "/version") ()
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "EZ Utils - Version")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/styles.css"))
     (:body
      (:div "Version")
      (:div "0.1")))))

(define-easy-handler (recipe-page :uri "/recipe") ()
  (flet  ((invoke-registered-ps-functions ()
            "pull all the registered ps functions from a global plist, then put them into a list"
            (do ((e *registered-ps-functions* (cddr e))
                 (result ()))
                ((null e) result)
              (push (getf *registered-ps-functions* (car e)) result))))
    (with-html-output-to-string
        (*standard-output* nil :prologue t :indent t)
      (:html
       (:head
        (:meta :charset "utf-8")
        (:title "Recipes")
        (:link :type "text/css"
               :rel "stylesheet"
               :href "/styles.css")
        (:script :type "text/javascript"
                       (str (jfh-web:define-ps-with-html-macro))
                       (str (share-server-side-constants))
                       (str (client-recipe))
                       (str (client-app-settings))
                       (str (client-ui-recipe))
                       (dolist (e (invoke-registered-ps-functions))
                         (str (funcall e)))))
       (:body
        (:div
         (:table :id "recipe-menu"))
        
        (:div :id "recipe-list"
              (:h1 "Recipe List")
              (:div
               (:p (:label "Recipe 1"))
               (:p (:label "Recipe 2"))))
        (:div :id "recipe-details" :hidden t
              (:h1 "Recipe 1")
              (:h2 "Ingredients")
              (:div
               (:p
                (:input :type "checkbox" "Ingredient 1"))
               (:p
                (:input :type "checkbox" "Ingredient 2")))
              (:h2 "Steps")
              (:div
               (:p
                (:input :type "checkbox" "Step 1"))
               (:p
                (:input :type "checkbox" "Step 2"))))
        (:div :id "recipe-entry" :hidden t
              (:h1 "Recipe Entry")
              (:div :id "recipe-entry-fields")))))))
             
