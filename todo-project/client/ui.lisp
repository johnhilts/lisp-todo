(in-package #:todo-project)

(defun client-ui ()
  "define client side UI functions"
  (ps

    (defparameter *todo-checkbox* "todo-check")
    (defparameter *todo-label* "todo-label")
    (defparameter *show-todo-edit* "show-todo-edit")
    (defparameter *hide-todo-edit* "hide-todo-edit")
    (defparameter *todo-text* "todo-text")

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
  (render-todo-filter)
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

(define-for-ps filter-todos ()
  (let* ((filter-text (@ (chain document (get-element-by-id "todo-filter-text")) value))
         (filtered-todos (chain todo-list (filter (lambda (todo) (chain (@ todo text) (match (new (-reg-exp filter-text "i")))))))))
    (render-todo-list filtered-todos))
  t)

(define-for-ps render-todo-filter ()
  "render html elements for todo filter"
  (let ((parent-element (chain document (get-element-by-id "todo-filter"))))
    (jfh-web::with-html-elements
        (div
         (div
          (span (br (ref . "br")))
          (input (id . "todo-filter-text") (type . "textbox") (placeholder . "Enter text to filter on here"))
          (span "  ")
          (button (onclick . "(filter-todos)") "Filter")))))
  t)

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
                      (show-todo-edit-class-name (+ *show-todo-edit* index))
                      (hide-todo-edit-class-name (+ *hide-todo-edit* index))
                      (todo-text-id (+ *todo-text* index))
                      (pre-style (if (@ todo done) "text-decoration: line-through;display:inline;" "display:inline;")))
                  (labels (
                           (show-input-for (todo show-edit)
                             "render text input for todo to edit it"
                             (let ((hide-todo-edit-elements (chain document (get-elements-by-class-name hide-todo-edit-class-name)))
                                   (show-todo-edit-elements (chain document (get-elements-by-class-name show-todo-edit-class-name)))
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
                           (id . "(echo todo-checkbox-id)")
                           (type . "checkbox")
                           (onclick . "(update-todo (echo index) (@ todo id)))")
                           (checked . "(@ todo done)")
                           (class . "(echo hide-todo-edit-class-name)"))
                          (span "  ")
                          (label
                           (id . "(echo todo-label-id)")
                           (for . "(echo todo-checkbox-id)")
                           (class . "(echo hide-todo-edit-class-name)")
                           (pre
                            (style . "(echo pre-style)")
                            (class . "(echo hide-todo-edit-class-name)") "(@ todo text)"))
                          (a (onclick . "(show-input-for todo t)") (class . "(echo hide-todo-edit-class-name)") "  ...")
                          (textarea (id . "(echo todo-text-id)") (hidden . "t") (rows . "5") (cols . "100") (class . "(echo show-todo-edit-class-name)"))
                          (span (br (ref . "(echo index)")))
                          (button (hidden . "t") (onclick . "(save-input-for todo)") (class . "(echo show-todo-edit-class-name)") "Save")
                          (span "  ")
                          (button (hidden . "t") (onclick . "(delete-todo todo)") (class . "(echo show-todo-edit-class-name)") "Delete")))))
                  t))))))


