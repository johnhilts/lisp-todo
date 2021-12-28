(in-package #:todo-project)

(setf (who:html-mode) :html5)

;; allow parenscript and cl-who to work together
(setf ps:*js-string-delimiter* #\")

(defparameter *static-root* (getf *system-settings* :static-root))

(defun make-todo-page (authenticated-user)
  "generate todo HTML page"
  ;; I split this calling of ps functions into 2 operations because str is a macrolet that's only available under with-html-output-to-string
  ;; I have options to consider such as I can mimic str and then put it whereever I want and then only have to work with 1 list
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:title "Todo List")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href (format-string  *static-root* "/styles.css?v=" (get-version)))
            (:script :type "text/javascript"
                     (who:str (jfh-web:define-ps-with-html-macro))
                     (who:str (share-server-side-constants))
                     (who:str(client-todo))
                     (who:str(client-app-settings))
                     (who:str(client-ui))
                     (dolist (e (invoke-registered-ps-functions))
                       (who:str(funcall e)))))
           (:body
            (:div :id "app-settings")
            (:div :id "todo-filter")
            (:div
             (:h1 (who:fmt "Todo List for ~a" authenticated-user)
                  (:div
                   (:textarea :id "todo-content" :placeholder "Enter Todo info here." :rows "5" :cols "100")
                   (:button :id "todo-add-btn" "Add")
                   (:button :id "todo-add-btn" :style "margin-left: 30px;" :onclick (who:str(ps-inline (setf (@ location href) "/import"))) "Import ..."))
                  (:div
                   (:table :id "todo-list"
                           (:thead (:th :id "todo-list-column-header" "To-do Items"))
                           (:tbody :id "todo-list-body" (:tr (:td "(To-do list empty)")))))))))))

(define-protected-page (todo-page "/todos") ()
  "HTTP endpoint for todo list"
  (make-todo-page authenticated-user))

(defvar *session-user-map* (make-hash-table))

(defun get-authenticated-user ()
  "get the authenticated user from server session."
  (format t "~&session value: ~a~%" (tbnl:session-value 'the-session))
  (gethash (tbnl:session-value 'the-session) *session-user-map*))

(defun establish-user-session (user-info)
  (let ((session-token (generate-unique-token)))
    (setf (tbnl:session-value 'the-session) session-token)
    (tbnl:set-cookie (string 'the-session) :value session-token :secure t :http-only t)
    (setf (gethash session-token *session-user-map*) (user-login user-info))))

(tbnl:define-easy-handler (authenticate :uri "/auth") (user password redirect-back-to)
  (flet ((show-auth-failure ()
           (who:with-html-output-to-string
               (*standard-output* nil :prologue t :indent t)
             (:html
              (:head
               (:meta :charset "utf-8")
               (:title "Auth Failure")
               (:link :type "text/css"
                      :rel "stylesheet"
                      :href (format-string  *static-root* "/styles.css?v=" (get-version))))
              (:body
               (:h2 "Authorization failed!")
               (:div "User or password didn't match"
                     (:a :href "/login" "Click here to try again!")))))))
    (let ((user-info (find-user-entry user :by :login)))
      (if
       (and user-info
            (string=
             (user-password user-info)
             (hash-password password)))
       (progn
         (establish-user-session user-info)
         (tbnl:redirect redirect-back-to))
       (show-auth-failure)))))

(tbnl:define-easy-handler (login-page :uri "/login") (redirect-back-to)
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:title "Todo List - Login")
      (:link :type "text/css"
             :rel "stylesheet"
             :href (format-string  *static-root* "/styles.css?v=" (get-version))))
     (:body
      (:h2 "Use this page to Login!")
      (:form :method "post" :action "auth"
             (:input :type "hidden" :name "redirect-back-to" :value (or redirect-back-to "/todos"))
             (:div :id "login-input-div"
              (:div (:input :name "user" :type "email" :placeholder "Login" :class "login-input"))
              (:div (:input :name "password" :type "password" :placeholder "Password" :class "login-input"))
              (:div (:button "Login") (:span "&nbsp;") (:button :id "sign-up-button" :type "button" :onclick "javascript:location.href=\"/signup\";" "Sign-Up"))))))))

(defun validate-signup-parameters (name user password confirm-password)
  (flet ((exists (user)
           (find-user-entry user :by :login)))
    (let ((signup-validation-failure-reasons ()))
      (if (or (zerop (length name)) (zerop (length user)) (zerop (length password)) (zerop (length confirm-password)))
          (push "Please enter all fields." signup-validation-failure-reasons))
      (progn
        (when (exists user)
          (push "User already exists; please login." signup-validation-failure-reasons))
        (when (not (string= password confirm-password))
          (push "Passwords don't match." signup-validation-failure-reasons)))
      (values
       (zerop (length signup-validation-failure-reasons))
       signup-validation-failure-reasons))))

(tbnl:define-easy-handler (signup-page :uri "/signup") ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:title "Todo List - Signup")
      (:link :type "text/css"
             :rel "stylesheet"
             :href (format-string  *static-root* "/styles.css?v=" (get-version))))
     (:body
      (if (or
           (tbnl:post-parameter "name")
           (tbnl:post-parameter "user")
           (tbnl:post-parameter "password")
           (tbnl:post-parameter "confirm-password"))
          (multiple-value-bind (signup-validation-successful signup-validation-failure-reasons)
              (validate-signup-parameters (tbnl:post-parameter "name") (tbnl:post-parameter "user") (tbnl:post-parameter "password") (tbnl:post-parameter "confirm-password"))
            (if signup-validation-successful
                (progn
                  (add-user (tbnl:post-parameter "name") (tbnl:post-parameter "user") (tbnl:post-parameter "password"))
                  (let ((user-info (find-user-entry (tbnl:post-parameter "user") :by :login)))
                    (establish-user-session user-info))
                  (who:htm (:script :type "text/javascript"
                                (who:str
                                 (ps:ps
                                   (alert "Signup Successful!")
                                   (setf (@ location href) "/todos"))))))
                (who:htm
                 (:div
                  (:span (who:fmt "Signup Failed, because <ul>~{<li>~a</li>~% ~}</ul>" signup-validation-failure-reasons)))
                 (:div
                  (:span "Please try again: ")
                  (:p (:a :href "/signup" "Back to Signup"))
                  (:p (:a :href "/login" "Back to Login"))))))
          (who:htm
           (:h2 "Use this page to sign-up!")
           (:div
            (:a :href "/login" "Back to Login"))
           (:form :method "post" :action "/signup"
                  (:div
                   (:div (:input :name "name" :type "text" :placeholder "Your Name" :class "login-input"))
                   (:div (:input :name "user" :type "email" :placeholder "Login" :class "login-input"))
                   (:div (:input :name "password" :type "password" :placeholder "Password" :class "login-input"))
                   (:div (:input :name "confirm-password" :type "password" :placeholder "Confirm Password" :class "login-input"))
                   (:div (:button "Submit"))))))))))

(tbnl:define-easy-handler (logout-page :uri "/logout") ()
  "logout endpoint"
  (format t "~&www-authorization: ~a, authorization: *** ~a ***~%" (tbnl:header-out :www-authenticate)(tbnl:header-out "authorization"))
  (remhash (tbnl:session-value 'the-session) *session-user-map*)
  (tbnl:delete-session-value 'the-session)
  (setf (tbnl:header-out :www-authenticate) nil)
  (tbnl:redirect "/login"))

(define-protected-page (admin-page "/admin") ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "Admin"))
     (:body
      (:h2 (who:fmt "Welcome to the Admin Page, ~a!" authenticated-user))
      (:div "You're supposed to be logged in to see this!")
      (:div
       (:a :href "/logout" "Click here to logout!"))))))

(defun get-version ()
  "0.5")

(tbnl:define-easy-handler (version-page :uri "/version") ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "EZ Utils - Version")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href (format-string  *static-root* "/styles.css?v=" (get-version))))
     (:body
      (:div "Version")
      (:div (who:str(get-version)))))))

(defun invoke-registered-ps-functions ()
  "pull all the registered ps functions from a global plist, then put them into a list"
  (do ((e *registered-ps-functions* (cddr e))
       (result ()))
      ((null e) result)
    (push (getf *registered-ps-functions* (car e)) result)))

(tbnl:define-easy-handler (recipe-page :uri "/recipe") ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:title "Recipes")
      (:link :type "text/css"
             :rel "stylesheet"
             :href (format-string  *static-root* "/styles.css?v=" (get-version)))
      (:script :type "text/javascript"
               (who:str(jfh-web:define-ps-with-html-macro))
               (who:str(share-server-side-constants))
               (who:str(client-recipe))
               (who:str(client-app-settings))
               (who:str(client-ui-recipe))
               (dolist (e (invoke-registered-ps-functions))
                 (who:str(funcall e)))))
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
            (:div :id "recipe-entry-fields"))))))

(defun make-import-todo-page ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "EZ Utils - Import")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href (format-string  *static-root* "/styles.css?v=" (get-version))))
     (:body
      (awhen (tbnl:post-parameter "import-list")
        (import-lines-into-todo-list it (tbnl:post-parameter "list-name"))
        (who:htm (:script :type "text/javascript"
                      (who:str
                       (ps:ps
                         (alert "Import Successful!")
                         (setf (@ location href) "/todos"))))))
      (:div
       (:h2 "Import to todo list")
       (:a :href "/todos" :style "margin-left: 10px;margin-bottom: 20px;" "back to todo list"))
      (:div
       (:form :method "post" :action "/import"
              (:input :name "list-name" :type "text" :placeholder "Enter List Name (optional)" :style "width: 200px;")
              (:br )
              (:br )
              (:textarea :name "import-list" :cols "100" :rows "35")
              (:div
               (:button "Import"))))))))

(define-protected-page (import-todo "/import") ()
  (make-import-todo-page))
