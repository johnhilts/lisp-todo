
(in-package #:todo-project)

(defun fetch-or-create-app-settings (&optional (get-user-data-path #'get-user-data-path))
  "read app-settings from persistence store; create default if doesn't exist yet"
  (let* ((user-data-path (if get-user-data-path (funcall get-user-data-path nil :by :login) ""))
         (default-app-settings  (list :hide-done-items 0))
         (call-back #'(lambda (app-settings) (if app-settings app-settings default-app-settings))))
    (fetch-or-create-data (concatenate 'string user-data-path "/" *app-settings-file-name*) call-back)))

(defun app-settings-data-get ()
  "get app settings and convert to json"
  (encode-plist-to-json-as-string (fetch-or-create-app-settings)))

(define-data-update-handler app-settings-data-update (model)
  "persist updated app settings"
  (let ((user-data-path (get-user-data-path nil :by :login)))
    (write-complete-file (concatenate 'string user-data-path "/" *app-settings-file-name*) model))
  (json:encode-json-to-string model))
