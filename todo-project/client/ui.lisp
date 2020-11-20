(in-package #:todo-project)

(defun client-ui ()
  "define client side UI functions"
  (ps

    (defparameter *todo-checkbox* "todo-check")
    (defparameter *todo-label* "todo-label")

    (defun clear-field (field)
      "clear input field's value"
      (setf (chain field value) "")
      t)
    
    (defun clear-children (parent-element)
      "remove all child nodes of a parent element"
      (while (chain parent-element (has-child-nodes))
        (chain parent-element (remove-child (@ parent-element first-child)))))
    
    (defun init ()
      "initialize html elements and JS objects on page load"
      (get-app-settings-from-server)
      (get-todo-list-from-server)
      (setf add-button (chain document
                              (get-element-by-id "todo-add-btn")))
      (chain add-button
             (add-event-listener "click" add-todo false)))

    (defun render-app-settings ()
      "render html elements for app settings"
      (let ((parent-element (chain document (get-element-by-id "app-settings"))))
        (jfh-web::with-html-elements
            (div
             (input (id . "hide-done") (type . "checkbox") (onclick . "(update-app-settings)") (checked . "(@ *app-settings* hide-done-items)"))
             (label (for . "hide-done") "Hide Done Items.")))))

    (defun render-todo-list (todo-list)
      "render html elements for todo list"
      (let* ((todo-list-table-body (chain document (get-element-by-id "todo-list-body")))
             (parent-element todo-list-table-body)
             (column-header (chain document (get-element-by-id "todo-list-column-header")))
             (count (length todo-list))
             (use-plural-form (or (> 1 count) (= 0 count))))
        (clear-children parent-element)
        (setf (chain column-header inner-text)
              (if use-plural-form "To-do Items" "To-do Item"))
        (chain todo-list
               (filter
                #'(lambda (todo)
                    (or (not (@ *app-settings* hide-done-items))
                        (not (@ todo done)))))
               (map
                #'(lambda (todo index)
                    (let ((todo-checkbox-id (+ *todo-checkbox* index))
                          (todo-label-id (+ *todo-label* index)))
                      (jfh-web::with-html-elements
                          (tr
                           (td
                            (input
                             (id . "(chain todo-checkbox-id (to-string))")
                             (type . "checkbox")
                             (onclick . "(update-todo (chain index (to-string)) (@ todo id)))")
                             (checked . "(@ todo done)"))
                            (label
                             (id . "(chain todo-label-id (to-string))")
                             (for . "(chain todo-checkbox-id (to-string))")
                             (style . "(if (@ todo done) \"text-decoration: line-through;\" \"\")") "(@ todo text)"))))
                      t))))))

    (setf (chain window onload) init)))
