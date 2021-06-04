
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

(defconstant +auth-token+ "abc123")

(defun is-authenticated ()
  (flet
      ((logged-in ()
         (multiple-value-bind (user password)
             (authorization)
           (and
            (equal user "nanook")
            (equal password "igloo")))))
    (or
     (session-value 'the-session)
     (logged-in))))

(define-easy-handler (signin-page :uri "/sign-in") (user password)
  (if
   (and
    (equal user "nanook")
    (equal password "igloo"))
   (progn
     (setf (session-value 'the-session) +auth-token+)
     (redirect "/admin"))
   (with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html
      (:head (:title "Auth Failure"))
      (:body
       (:h2 "Authorization failed!")
       (:div "User or password didn't match"
             (:a :href "/auth" "Click here to try again!")))))))

(define-easy-handler (authenticate-page :uri "/auth") ()
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "Auth"))
     (:body
      (:h2 "Use this page to sign-in!")
      (:form :method "post" :action "sign-in"
             (:div
              (:div (:input :name "user" :type "text" :placeholder "Login"))
              (:div (:input :name "password" :type "password" :placeholder "Password"))
              (:div (:button "Login"))))))))

(define-easy-handler (logout-page :uri "/logout") ()
  "logout endpoint"
  (delete-session-value 'the-session)
  (redirect "/auth"))

(define-easy-handler (admin-page :uri "/admin") ()
  "dummy admin page"
  (if (is-authenticated)
      (with-html-output-to-string
          (*standard-output* nil :prologue t :indent t)
        (:html
         (:head (:title "Admin"))
         (:body
          (:h2 "Welcome to the Admin Page!")
          (:div "You're supposed to be logged in to see this!")
          (:div
           (:a :href "/logout" "Click here to logout!")))))
      (require-authorization)))

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

(defun get-version ()
  "0.5")

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
      (:div (str (get-version)))))))

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
              (:div :id "recipe-list-entries"))
        (:div :id "recipe-details" :hidden t
              (:div :id "recipe-detail-name")
              (:h2 "Ingredients")
              (:div :id "recipe-ingredients")
              (:h2 "Steps")
              (:div :id "recipe-steps"))
        (:div :id "recipe-entry" :hidden t
              (:h1 "Recipe Entry")
              (:div :id "recipe-entry-fields")))))))
             
