
(in-package #:todo-project)

(defun fetch-or-create-app-settings ()
  "read app-settings from persistence store; create default if doesn't exist yet"
  (let* ((default-app-settings  (list :hide-done-items 0))
         (call-back #'(lambda (app-settings) (if app-settings app-settings default-app-settings))))
    (fetch-or-create-data *app-settings-file-path* call-back)))

(defun app-settings-data-get ()
  "get app settings and convert to json"
  (encode-plist-to-json-as-string (fetch-or-create-app-settings)))

(define-data-update-handler app-settings-data-update (model)
  "persist updated app settings"
  (write-complete-file *app-settings-file-path* model)
  (json:encode-json-to-string model))
