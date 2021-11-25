
(in-package #:todo-project)

(setf (html-mode) :html5)

;; allow parenscript and cl-who to work together
(setf *js-string-delimiter* #\")

(defun make-todo-page (authenticated-user)
  "generate todo HTML page"
  ;; I split this calling of ps functions into 2 operations because str is a macrolet that's only available under with-html-output-to-string
  ;; I have options to consider such as I can mimic str and then put it whereever I want and then only have to work with 1 list
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
           (:head
            (:meta :charset "utf-8")
            (:title "Todo List")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href  (str (format nil "/styles.css?v=~a" (get-version))))
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
             (:h1 (fmt "Todo List for ~a" authenticated-user)
                  (:div
                   (:textarea :id "todo-content" :placeholder "Enter Todo info here." :rows "5" :cols "100")
                   (:button :id "todo-add-btn" "Add")
                   (:button :id "todo-add-btn" :style "margin-left: 30px;" :onclick (str (ps-inline (setf (@ location href) "/import"))) "Import ..."))
                  (:div
                   (:table :id "todo-list"
                           (:thead (:th :id "todo-list-column-header" "To-do Items"))
                           (:tbody :id "todo-list-body" (:tr (:td "(To-do list empty)")))))))))))

(define-protected-page (todo-page "/todos") ()
  "HTTP endpoint for todo list"
  (make-todo-page authenticated-user))

(defvar *session-user-map* (make-hash-table))

(defun get-authenticated-user ()
  (format t "~&session value: ~a~%" (session-value 'the-session))
  (gethash (session-value 'the-session) *session-user-map*))

(define-easy-handler (authenticate :uri "/auth") (user password redirect-back-to)
  (let ((user-info (find-user-entry user :by :login)))
    (if
     (and
      user-info
      (equal (user-password user-info) password))
     (let ((session-token (generate-unique-token)))
       (setf (session-value 'the-session) session-token)
       (set-cookie (string 'the-session) :value session-token :secure t :http-only t)
       (setf (gethash session-token *session-user-map*) (user-login user-info))
       (redirect redirect-back-to))
     (with-html-output-to-string
         (*standard-output* nil :prologue t :indent t)
       (:html
        (:head (:title "Auth Failure"))
        (:body
         (:h2 "Authorization failed!")
         (:div "User or password didn't match"
               (:a :href "/login" "Click here to try again!"))))))))

(define-easy-handler (login-page :uri "/login") (redirect-back-to)
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:title "Todo List - Login")
      (:link :type "text/css"
             :rel "stylesheet"
             :href (str (format nil "/styles.css?v=~a" (get-version)))))
     (:body
      (:h2 "Use this page to Login!")
      (:form :method "post" :action "auth"
             (:input :type "hidden" :name "redirect-back-to" :value (or redirect-back-to "/todos"))
             (:div :id "login-input-div"
              (:div (:input :name "user" :type "text" :placeholder "Login" :class "login-input"))
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

(define-easy-handler (signup-page :uri "/signup") ()
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:title "Todo List - Signup")
      (:link :type "text/css"
             :rel "stylesheet"
             :href (str (format nil "/styles.css?v=~a" (get-version)))))
     (:body
      (if (or
           (post-parameter "name")
           (post-parameter "user")
           (post-parameter "password")
           (post-parameter "confirm-password"))
          (multiple-value-bind (signup-validation-successful signup-validation-failure-reasons)
              (validate-signup-parameters (post-parameter "name") (post-parameter "user") (post-parameter "password") (post-parameter "confirm-password"))
            (if signup-validation-successful
                (progn
                  (add-user (post-parameter "name") (post-parameter "user") (post-parameter "password"))
                  (htm (:script :type "text/javascript"
                              (str
                               (ps
                                 (alert "Signup Successful!")
                                 (setf (@ location href) "/todos"))))))
                (htm
                 (:div
                  (:span (fmt "Signup Failed, because <ul>~{<li>~a</li>~% ~}</ul>" signup-validation-failure-reasons)))
                 (:div
                  (:span "Please try again: ")
                  (:p (:a :href "/signup" "Back to Signup"))
                  (:p (:a :href "/login" "Back to Login"))))))
          (htm
           (:h2 "Use this page to sign-up!")
           (:div
            (:a :href "/login" "Back to Login"))
           (:form :method "post" :action "/signup"
                  (:div
                   (:div (:input :name "name" :type "text" :placeholder "Your Name" :class "login-input"))
                   (:div (:input :name "user" :type "text" :placeholder "Login" :class "login-input"))
                   (:div (:input :name "password" :type "password" :placeholder "Password" :class "login-input"))
                   (:div (:input :name "confirm-password" :type "password" :placeholder "Confirm Password" :class "login-input"))
                   (:div (:button "Submit"))))))))))

(define-easy-handler (logout-page :uri "/logout") ()
  "logout endpoint"
  (format t "~&www-authorization: ~a, authorization: *** ~a ***~%" (header-out :www-authenticate)(header-out "authorization"))
  (remhash (session-value 'the-session) *session-user-map*)
  (delete-session-value 'the-session)
  (setf (header-out :www-authenticate) nil)
  (redirect "/login"))

(defmacro define-protected-page (name-and-end-point params &body body)
  "macro to DRY pages requiring authentication"
  (let* ((name (car name-and-end-point))
        (end-point (cadr name-and-end-point))
        (possible-description (car body))
        (has-description (and (atom possible-description) (stringp possible-description)))
        (description (if has-description possible-description nil))
        (body-after-description (if has-description (cdr body) body)))
    `(define-easy-handler (,name :uri ,end-point) (,@params)
       ,(when description description)
        (multiple-value-bind (authenticated-user present-p)
           (get-authenticated-user)
         (if present-p
             ,@body-after-description
             (redirect (format nil "/login?redirect-back-to=~a" (url-encode ,end-point))))))))

(define-protected-page (admin-page "/admin") ()
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "Admin"))
     (:body
      (:h2 (fmt "Welcome to the Admin Page, ~a!" authenticated-user))
      (:div "You're supposed to be logged in to see this!")
      (:div
       (:a :href "/logout" "Click here to logout!"))))))

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

(defun invoke-registered-ps-functions ()
  "pull all the registered ps functions from a global plist, then put them into a list"
  (do ((e *registered-ps-functions* (cddr e))
       (result ()))
      ((null e) result)
    (push (getf *registered-ps-functions* (car e)) result)))

(define-easy-handler (recipe-page :uri "/recipe") ()
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
            (:div :id "recipe-entry-fields"))))))

(defun make-import-todo-page ()
  (with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head (:title "EZ Utils - Import")
            (:link :type "text/css"
                   :rel "stylesheet"
                   :href "/styles.css"))
     (:body
      (awhen (post-parameter "import-list")
        (import-lines-into-todo-list it (post-parameter "list-name"))
        (htm (:script :type "text/javascript"
                      (str
                       (ps
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

(define-easy-handler (import-todo :uri "/import") ()
  (make-import-todo-page))

