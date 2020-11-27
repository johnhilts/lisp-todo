(in-package #:todo-project)

(defun client-ui ()
  "define client side UI functions"
  (ps

    (defparameter *todo-checkbox* "todo-check")
    (defparameter *todo-label* "todo-label")
    (defparameter *todo-anchor* "todo-anchor")
    (defparameter *todo-text* "todo-text")
    (defparameter *todo-save-button* "todo-save-button")
    (defparameter *todo-delete--button* "todo-delete-button")

    (setf (chain window onload) init)))

(define-for-ps clear-field (field)
  "clear input field's value"
  (setf (chain field value) "")
  t)
    
(define-for-ps clear-children (parent-element)
  "remove all child nodes of a parent element"
  (while (chain parent-element (has-child-nodes))
    (chain parent-element (remove-child (@ parent-element first-child)))))
    
(define-for-ps init ()
  "initialize html elements and JS objects on page load"
  (get-app-settings-from-server)
  (get-todo-list-from-server)
  (setf add-button (chain document
                          (get-element-by-id "todo-add-btn")))
  (chain add-button
         (add-event-listener "click" add-todo false)))

(define-for-ps render-app-settings ()
  "render html elements for app settings"
  (let ((parent-element (chain document (get-element-by-id "app-settings"))))
    (jfh-web::with-html-elements
        (div
         (input (id . "hide-done") (type . "checkbox") (onclick . "(update-app-settings)") (checked . "(@ *app-settings* hide-done-items)"))
         (label (for . "hide-done") "Hide Done Items.")))))

(define-for-ps render-todo-list (todo-list)
  "render html elements for todo list"
  (let* ((todo-list-table-body (chain document (get-element-by-id "todo-list-body")))
         (parent-element todo-list-table-body)
         (column-header (chain document (get-element-by-id "todo-list-column-header")))
         (filtered-todos (chain todo-list (filter
                                           #'(lambda (todo)
                                               (or (not (@ *app-settings* hide-done-items))
                                                   (not (@ todo done)))))))
         (count (length filtered-todos))
         (use-plural-form (or (> count 1) (= 0 count))))
    (clear-children parent-element)
    (setf (chain column-header inner-text)
          (if use-plural-form "To-do Items" "To-do Item"))
    (chain filtered-todos
           (map
            #'(lambda (todo index)
                (let ((todo-checkbox-id (+ *todo-checkbox* index))
                      (todo-label-id (+ *todo-label* index))
                      (todo-anchor-id (+ *todo-anchor* index))
                      (todo-text-id (+ *todo-text* index))
                      (todo-save-button-id (+ *todo-save-button* index))
                      (todo-delete-button-id (+ *todo-delete-button* index)))
                  (labels (
                           (show-input-for (todo show-edit)
                             "render text input for todo to edit it"
                             (let ((hide-todo-edit-elements (chain document (get-elements-by-class-name "hide-todo-edit")))
                                   (show-todo-edit-elements (chain document (get-elements-by-class-name "show-todo-edit")))
                                   (todo-text-element (chain document (get-element-by-id todo-text-id))))
                               (dolist (e hide-todo-edit-elements) (setf (@ e hidden) show-edit))
                               (dolist (e show-todo-edit-elements) (setf (@ e hidden) (not show-edit)))
                               (setf (@ todo-text-element value) (@ todo text))
                               (chain todo-text-element (focus)))
                             t)
                           (save-input-for (todo)
                             (let* ((todo-text-element (chain document (get-element-by-id todo-text-id)))
                                    (updated-text (@ todo-text-element value)))
                               (setf (@ todo text) updated-text)
                               (update-todo-from-edit todo)
                               (show-input-for todo false))
                             t)
                           (delete-todo (todo)
                             (delete-todo-by-id (@ todo id))
                             (show-input-for todo false)
                             t))
                    (jfh-web::with-html-elements
                        (tr
                         (td
                          (input
                           (id . "(chain todo-checkbox-id (to-string))")
                           (type . "checkbox")
                           (onclick . "(update-todo (chain index (to-string)) (@ todo id)))")
                           (checked . "(@ todo done)")
                           (class . "hide-todo-edit"))
                          (span "  ")
                          (label
                           (id . "(chain todo-label-id (to-string))")
                           (for . "(chain todo-checkbox-id (to-string))")
                           (style . "(if (@ todo done) \"text-decoration: line-through;\" \"\")")
                           (class . "hide-todo-edit") "(@ todo text)")
                          (a (id . "(chain todo-anchor-id (to-string))") (onclick . "(show-input-for todo t)") (class . "hide-todo-edit") "  ...")
                          (textarea (id . "(chain todo-text-id (to-string))") (hidden . "t") (rows . "5") (cols . "100") (class . "show-todo-edit"))
                          (span "  ")
                          (button (id . "(chain todo-save-button-id (to-string))") (hidden . "t") (onclick . "(save-input-for todo)") (class . "show-todo-edit") "Save")
                          (span "  ")
                          (button (id . "(chain todo-delete-button-id (to-string))") (hidden . "t") (onclick . "(delete-todo todo)") (class . "show-todo-edit") "Delete")))))
                  t))))))


