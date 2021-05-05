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
    (render-todo-list filtered-todos)
    (update-app-settings))
  t)

(define-for-ps render-todo-filter ()
  "render html elements for todo filter"
  (let ((parent-element (chain document (get-element-by-id "todo-filter"))))
    (jfh-web::with-html-elements
        (div
         (div
          (span (br (ref . "br")))
          (input (id . "todo-filter-text") (type . "textbox") (placeholder . "Enter text to filter on here") (value . "(@ *app-settings* filter-text)")))
         (span "  ")
         (button (onclick . "(filter-todos)") "Filter"))))
  t)

(define-for-ps render-todo-list (todo-list)
  "render html elements for todo list"
  (let* ((todo-list-table-body (chain document (get-element-by-id "todo-list-body")))
         (parent-element todo-list-table-body)
         (column-header (chain document (get-element-by-id "todo-list-column-header")))
         (filter-text (@ (chain document (get-element-by-id "todo-filter-text")) value))
         (filtered-todos (chain todo-list (filter
                                           #'(lambda (todo)
                                               (and
                                                (or (not (@ *app-settings* hide-done-items))
                                                    (not (@ todo done)))
                                                (chain (@ todo text) (match (new (-reg-exp filter-text "i")))))))))
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
                             (when (confirm "Are you sure you want to remove this?")
                               (delete-todo-by-id (@ todo id)))
                             (show-input-for todo false)
                             t))
                    (jfh-web::with-html-elements
                        (tr
                         (td
                          (input
                           (id . "(progn todo-checkbox-id)")
                           (type . "checkbox")
                           (onclick . "(update-todo (progn index) (@ todo id)))")
                           (checked . "(@ todo done)")
                           (class . "(progn hide-todo-edit-class-name)"))
                          (span "  ")
                          (label
                           (id . "(progn todo-label-id)")
                           (for . "(progn todo-checkbox-id)")
                           (class . "(progn hide-todo-edit-class-name)")
                           (pre
                            (style . "(progn pre-style)")
                            (class . "(progn hide-todo-edit-class-name)") "(@ todo text)"))
                          (a (onclick . "(show-input-for todo t)") (class . "(progn hide-todo-edit-class-name)") "  ...")
                          (textarea (id . "(progn todo-text-id)") (hidden . "t") (rows . "5") (cols . "100") (class . "(progn show-todo-edit-class-name)"))
                          (span (br (ref . "(progn index)")))
                          (button (hidden . "t") (onclick . "(save-input-for todo)") (class . "(progn show-todo-edit-class-name)") "Save")
                          (span "  ")
                          (button (hidden . "t") (onclick . "(delete-todo todo)") (class . "(progn show-todo-edit-class-name)") "Delete")))))
                  t))))))

(defun client-ui-recipe ()
  "define client side UI functions"
  (ps

    (defparameter *recipe-list* "recipe-list")
    (defparameter *recipe-entry* "recipe-entry")
    (defparameter *recipe-details* "recipe-details")
    ;; (defparameter *todo-label* "todo-label")
    ;; (defparameter *show-todo-edit* "show-todo-edit")
    ;; (defparameter *hide-todo-edit* "hide-todo-edit")
    ;; (defparameter *todo-text* "todo-text")
    
    (setf (chain window onload) init-recipe)))

(define-for-ps init-recipe ()
  "initialize html elements and JS objects on page load"
  (with-callback
      (get-recipe-list-from-server)
    ;; (setf add-button (chain document
    ;;                         (get-element-by-id "todo-add-btn")))
    ;; (chain add-button
    ;;        (add-event-listener "click" add-todo false))
    (render-recipe-menu)
    (render-recipe-list)))

(define-for-ps get-recipe-sections ()
  (let ((list-section (chain document (get-element-by-id *recipe-list*)))
        (add-section (chain document (get-element-by-id *recipe-entry*)))
        (detail-section (chain document (get-element-by-id *recipe-details*))))
    (values list-section add-section detail-section)))

(define-for-ps can-hide-recipe-section (el1 el2-id)
  (not (string= (@ el1 id) el2-id)))

(define-for-ps display-recipe-section (section)
  (multiple-value-bind
        (list-section add-section detail-section)
      (get-recipe-sections)
    (setf (@ list-section hidden) (can-hide-recipe-section list-section section)
          (@ add-section hidden) (can-hide-recipe-section add-section section)
          (@ detail-section hidden) (can-hide-recipe-section detail-section section)))
  t)

(define-for-ps render-recipe-list ()
  (display-recipe-section *recipe-list*)
  (let* ((recipe-list-entries (chain document (get-element-by-id "recipe-list-entries")))
         (parent-element recipe-list-entries))
    (clear-children parent-element)
    (chain recipe-list
           (map
            #'(lambda (recipe)
                (jfh-web::with-html-elements
                    (p
                     (p (a (href . "#") "(@ recipe name)"))))))))
  t)

(define-for-ps render-recipe-add ()
  (display-recipe-section *recipe-entry*)
  (let* ((recipe-entry-fields (chain document (get-element-by-id "recipe-entry-fields")))
         (parent-element recipe-entry-fields))
    (jfh-web::with-html-elements
        (p
         (p
          (input (type . "text") (id . "recipe-name") (placeholder . "Recipe Name")))
         (p
          (textarea (id . "recipe-ingredients") (placeholder . "Ingredients") (rows . "10") (cols . "100")))
         (p
          (textarea (id . "recipe-steps") (placeholder . "Steps") (rows . "10") (cols . "100")))
         (p
          (button (onclick . "(add-recipe)") "Add Recipe")))))
  t)

(define-for-ps render-recipe-detail ()
  (display-recipe-section *recipe-details*)
  (flet ((display-ingredients ()
           (let* ((recipe-ingredients (chain document (get-element-by-id "recipe-ingredients")))
                  (parent-element recipe-ingredients))
             (clear-children parent-element)
             (chain (@ (aref recipe-list 0) :ingredients)
                    (map
                     #'(lambda (ingredient index)
                         (let ((checkbox-id (concatenate 'string "ingredient-" index)))
                           (jfh-web::with-html-elements
                               (p
                                (input (id . "(progn checkbox-id)") (type . "checkbox"))
                                (label (for . "(progn checkbox-id)") ingredient)))))))))
         (display-steps ()
           (let* ((recipe-steps (chain document (get-element-by-id "recipe-steps")))
                  (parent-element recipe-steps))
             (clear-children parent-element)
             (chain (@ (aref recipe-list 0) :steps)
                    (map
                     #'(lambda (step index)
                         (let ((checkbox-id (concatenate 'string "step-" index)))
                           (jfh-web::with-html-elements
                               (p
                                (input (id . "(progn checkbox-id)") (type . "checkbox"))
                                (label (for . "(progn checkbox-id)") step))))))))))
    (display-ingredients)
    (display-steps))
  t)

(define-for-ps render-section (section-name)
  (case section-name
    ("list"
     (render-recipe-list))
    ("add"
     (render-recipe-add))
    ("detail"
     (render-recipe-detail))))

(define-for-ps render-recipe-menu ()
  (let* ((recipe-menu (chain document (get-element-by-id "recipe-menu")))
         (parent-element recipe-menu))
    (jfh-web::with-html-elements
        (tr
         (td (span (a (onclick . "(render-section 'list)") "List")))
         (td (span (a (onclick . "(render-section 'add)") "Add")))))))
