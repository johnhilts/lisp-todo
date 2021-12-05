(in-package #:todo-project)

(defun fetch-or-create-system-settings ()
  "read system-settings from persistence store; create default if doesn't exist yet"
  (let* ((default-system-settings  (list :use-ssl nil))
        (call-back #'(lambda (system-settings) (if system-settings system-settings default-system-settings))))
    (fetch-or-create-data *system-settings-file-path* call-back)))
