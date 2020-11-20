
(in-package #:todo-project)

(defun fetch-or-create-app-settings ()
  "read app-settings from persistence store; create default if doesn't exist yet"
  (let ((app-settings (read-complete-file "./app-settings-list.sexp")))
    (if app-settings
        app-settings
        (list :hide-done-items 0))))

(defun get-app-settings ()
  "get app settings and convert to json"
  ;; todo - put the string replace inside of encode-plist-to-json-as-string (it's my function!)
  (string-replace (encode-plist-to-json-as-string (fetch-or-create-app-settings)) "\"hideDoneItems\":0" "\"hideDoneItems\":false"))

(defun app-settings-data-get ()
  "wrapper for app settings"
  (get-app-settings))

(defun convert-input-to-app-settings (input)
  "convert json input to app-specific format"
  (reduce #'join-pairs  input :initial-value ()))

(defun app-settings-data-update (raw-data)
  "persist updated app settings"
  (let ((updated-app-settings (convert-input-to-app-settings (json:decode-json-from-string raw-data))))
    (write-complete-file "./app-settings-list.sexp" updated-app-settings)
    (json:encode-json-to-string updated-app-settings)))
