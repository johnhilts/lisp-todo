(in-package #:todo-project)

(setf (who:html-mode) :html5)

;; allow parenscript and cl-who to work together
(setf ps:*js-string-delimiter* #\")

(defparameter *static-root* (getf *system-settings* :static-root))

(defun get-app-menu ()
  (who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:div :style "float: right;"
          (:span (:a :href "/todos" "Todo List") "&nbsp" (:a :href "/recipe" "Recipes")))))

(defun make-todo-page (authenticated-user)
  "generate todo HTML page"
  ;; I split this calling of ps functions into 2 operations because str is a macrolet that's only available under with-html-output-to-string
  ;; I have options to consider such as I can mimic str and then put it whereever I want and then only have to work with 1 list
  (with-app-layout "EZ Utils - Todo List" (client-todo client-app-settings client-ui) 
    (:body
     (:div :id "app-settings"
           (who:str (get-app-menu)))
     (:div
      (:h1 (who:fmt "Todo List for ~a" authenticated-user))
      (:div :id "todo-list-area"
       (:h2
        (:table :id "todo-list"
                (:thead (:th :id "todo-list-column-header" "To-do Items"))
                (:tbody :id "todo-list-body" (:tr (:td "(To-do list empty)"))))))
      (:div :id "todo-filter")
      (:div :id "filter-tag-content" :hidden "true")
      (:div :id "new-todo-tag-content" :hidden "true")
      (:div :id "todo-new-entry-and-import"
       (:textarea :id "todo-content" :placeholder "Enter Todo info here." :rows "5" :cols "100")
       (:div :id "new-todo-tag-content" :hidden "true"
             (:div :id "new-todo-tag-candidates" :class "tag-display"))
       (:button :id "todo-add-btn" "Add")
       (:button :style "margin-left: 30px;" :onclick (who:str(ps-inline (setf (@ location href) "/import"))) "Import ..."))))))

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
               (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
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
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
      (:title "Todo List - Login")
      (:link :type "text/css"
             :rel "stylesheet"
             :href (format-string  *static-root* "/styles.css?v=" (get-version))))
     (:body
      (:h2 "Use this page to Login!")
      (:form :method "post" :action "auth"
             (:input :type "hidden" :name "redirect-back-to" :value (or redirect-back-to "/todos"))
             (:div :id "login-input-div"
              (:div (:input :name "user" :type "email" :placeholder "Login" :class "login-input" :autofocus "autofocus"))
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
      (:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
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
                   (:div (:input :name "name" :type "text" :placeholder "Your Name" :class "login-input" :autofocus "autofocus"))
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
  "0.9")

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

(define-protected-page (recipe-page "/recipe") ()
  (with-app-layout "Recipes" (client-recipe client-app-settings client-ui-recipe) 
    (:body
     (:div
      (who:str (get-app-menu))
      (:table :id "recipe-menu"))
     (:div :id "recipe-list"
           (:h1 (who:fmt "Recipe List for ~a" authenticated-user))
           (:div :id "recipe-list-entries"))
     (:div :id "recipe-details" :hidden t
           (:div :id "recipe-detail-name")
           (:h2 "Ingredients")
           (:div :id "recipe-ingredients")
           (:h2 "Steps")
           (:div :id "recipe-steps"))
     (:div :id "recipe-entry" :hidden t
           (:h1 "Recipe Entry")
           (:div :id "recipe-entry-fields")))))

(defun make-import-todo-page ()
  (with-app-layout "EZ Utils - Todo Import" (client-import client-app-settings client-ui-import)
    (:body
     (awhen (tbnl:post-parameter "import-list")
       (let ((new-todo-ids (import-lines-into-todo-list it)))
         (awhen (tbnl:post-parameter "import-selected-tags")
           (import-todo-tags-list it new-todo-ids)))
       (who:htm (:script :type "text/javascript"
                         (who:str
                          (ps:ps
                           (setf (@ location href) "/todos"))))))
     (:div
      (:h2 "Import to To-do List")
      (:a :href "/todos" :style "margin-left: 10px;margin-bottom: 20px;" "back to todo list"))
     (:div
      (:form :method "post" :action "/import"
             (:div :id "import-todo-tag-content" :hidden "true"
                   (:div :id "import-todo-tag-candidates" :class "tag-display" :style "border-color: green; border-style: solid;"))
             (:p (:span "Each line will be imported as a separate To-Do Item."))
             (:textarea :id "import-list" :name "import-list" :cols "100" :rows "25")
             (:input :type "hidden" :id "import-selected-tags" :name "import-selected-tags")
             (:div
              (:button "Import")))))))

(define-protected-page (import-todo "/import") ()
  (make-import-todo-page))
