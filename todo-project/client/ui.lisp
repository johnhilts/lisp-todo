(in-package #:todo-project)

(defun client-ui ()
  "define client side UI functions"
  (ps:ps

    (defparameter *todo-checkbox* "todo-check")
    (defparameter *todo-label* "todo-label")
    (defparameter *show-todo-edit* "show-todo-edit")
    (defparameter *hide-todo-edit* "hide-todo-edit")
    (defparameter *todo-text* "todo-text")
    (defparameter *candidate-tag-text* "candidate-tag")
    (defparameter *selected-tag-text* "selected-tag")

    (defparameter *selected-tag-ids* (list))

    (setf (ps:chain window onload) init)))

(ps:ps
  (defmacro with-callback (fn &body body)
    `(,(car fn) ,@(cdr fn) #'(lambda (),@body))))

(define-for-ps clear-field (field)
  "clear input field's value"
  (setf (ps:chain field value) "")
  t)
    
(define-for-ps clear-children (parent-element)
  "remove all child nodes of a parent element"
  (while (ps:chain parent-element (has-child-nodes))
    (ps:chain parent-element (remove-child (@ parent-element first-child)))))
    
(define-for-ps init ()
  "initialize html elements and JS objects on page load"
  (with-callback
      (get-app-settings-from-server)
    (get-todo-list-from-server))
  (let ((add-button (ps:chain document (get-element-by-id "todo-add-btn")))
        (todo-content (ps:chain document (get-element-by-id "todo-content"))))
    (ps:chain add-button (add-event-listener "click" add-todo false))
    (ps:chain todo-content (add-event-listener "keypress" render-tag-content todo-content event false))))

(define-for-ps render-app-settings ()
  "render html elements for app settings"
  (let ((parent-element (ps:chain document (get-element-by-id "app-settings"))))
    (jfh-web::with-html-elements
        (div
         (input (id . "hide-done") (type . "checkbox") (onclick . "(update-app-settings)") (checked . "(@ *app-settings* hide-done-items)"))
         (label (for . "hide-done") "Hide Done Items.")))))

(define-for-ps filter-todos ()
  (let* ((filter-text (@ (ps:chain document (get-element-by-id "todo-filter-text")) value))
         (filtered-todos (ps:chain todo-list (filter (lambda (todo) (ps:chain (@ todo text) (match (new (-reg-exp filter-text "i")))))))))
    (render-todo-list filtered-todos)
    (update-app-settings))
  t)

(define-for-ps render-todo-filter ()
  "render html elements for todo filter"
  (let ((parent-element (ps:chain document (get-element-by-id "todo-filter"))))
    (jfh-web::with-html-elements
        (div
         (div
          (span (br (ref . "br")))
          (input (id . "todo-filter-text") (type . "textbox") (placeholder . "Enter text to filter on here") (value . "(@ *app-settings* filter-text)")))
         (span "  ")
         (button (onclick . "(filter-todos)") "Filter"))))
  t)

(define-for-ps get-tag-content-area-element ()
  "get the element that contains the tag area"
  (ps:chain document (get-element-by-id "tag-content")))

(define-for-ps render-selected-tags (selected-tag-ids)
  "render the tags selected to go with the current todo item"
  (let* ((parent-element (ps:chain document (get-element-by-id "selected-tags")))
        (selected-tags (ps:chain selected-tag-ids (map #'(lambda (selected-tag-id) (ps:chain *taglist* (find #'(lambda (tag) (= (ps:@ tag id) selected-tag-id))))))))
        (selected-tags-element parent-element))
    (when selected-tags-element
      (clear-children parent-element))
    (ps:chain selected-tags
              (map
               #'(lambda (tag)
                   (let ((tag-id (+ *selected-tag-text* (ps:@ tag id))))
                     (jfh-web::with-html-elements
                         (span
                          (style . "margin: 5px;")
                          (a (id . "(progn tag-id)") (onclick . "(remove-tag-from-todo tag)") "(ps:@ tag text)"))))
                   t))))
  t)

(define-for-ps add-tag-to-todo (tag)
  (let* ((tag-id (ps:@ tag id))
         (tag-list-element-id (+ *candidate-tag-text* tag-id)))
    (ps:chain *selected-tag-ids* (push tag-id))
    (ps:chain (ps:chain document (get-element-by-id tag-list-element-id)) (remove))
    (render-selected-tags *selected-tag-ids*))
  t)

(define-for-ps remove-tag-from-todo (tag)
  (let* ((tag-id (ps:@ tag id))
         (tag-list-element-id (+ *candidate-tag-text* tag-id)))
    (ps:chain *selected-tags* (push tag-id))
    (ps:chain (ps:chain document (get-element-by-id tag-list-element-id)) (remove)))
  t)

(define-for-ps render-tag-content (input event)
  "render tag entry, selected tags, and tag candidates"
  (let ((tag-content-area (get-tag-content-area-element))
        (tag-candidate-area (ps:chain document (get-element-by-id "tag-candidates"))))
    (flet ((tag-content-visible ()
             (not (ps:@ tag-content-area hidden)))
           (render-tag-candidates ()
             (let ((most-recent-candidates (ps:chain *tag-list* (slice 0 3)))
                   (parent-element tag-candidate-area))
               (ps:chain most-recent-candidates
                         (map
                          #'(lambda (candidate-tag)
                              (let ((candidate-tag-id (+ *candidate-tag-text* (ps:@ candidate-tag id))))
                                (jfh-web::with-html-elements
                                    (span
                                     (style . "margin: 5px;")
                                     (a (id . "(progn candidate-tag-id)") (onclick . "(add-tag-to-todo candidate-tag)") "(ps:@ candidate-tag text)"))))
                              t))))
             t)             
           (render-tag-entry ()
             (let ((parent-element tag-candidate-area))
               (jfh-web::with-html-elements
                   (div (input (type . "text") (id . "tag-input") (placeholder . "add new tag"))
                        (div (id . "selected-tags")))))
             t))
      (unless (tag-content-visible)
        (setf (ps:@ tag-content-area hidden) ps:f)
        (render-tag-candidates)
        (render-tag-entry)
        ;; (render-selected-tags *selected-tag-ids*)
        )))
  t)

(define-for-ps render-todo-list (todo-list)
  "render html elements for todo list"
  (let* ((todo-list-table-body (ps:chain document (get-element-by-id "todo-list-body")))
         (parent-element todo-list-table-body)
         (column-header (ps:chain document (get-element-by-id "todo-list-column-header")))
         (filter-text (@ (ps:chain document (get-element-by-id "todo-filter-text")) value))
         (filtered-todos (ps:chain todo-list (filter
                                           #'(lambda (todo)
                                               (and
                                                (or (not (@ *app-settings* hide-done-items))
                                                    (not (@ todo done)))
                                                (ps:chain (@ todo text) (match (new (-reg-exp filter-text "i")))))))))
         (count (length filtered-todos))
         (use-plural-form (or (> count 1) (= 0 count)))
         (checked-count (length (ps:chain filtered-todos (filter #'(lambda (todo) (@ todo done)))))))
    (clear-children parent-element)
    (setf (ps:chain column-header inner-text)
          (+ (if use-plural-form "To-do Items" "To-do Item") " " (ps:chain checked-count (to-string)) "/" (ps:chain count (to-string))))
    (ps:chain filtered-todos
           (map
            #'(lambda (todo index)
                (let ((todo-checkbox-id (+ *todo-checkbox* index))
                      (todo-label-id (+ *todo-label* index))
                      (show-todo-edit-class-name (+ *show-todo-edit* index))
                      (hide-todo-edit-class-name (+ *hide-todo-edit* index))
                      (todo-text-id (+ *todo-text* index))
                      (pre-style (if (@ todo done) "text-decoration: line-through;color: #888;display:inline;" "display:inline;")))
                  (labels (
                           (show-input-for (todo show-edit)
                             "render text input for todo to edit it"
                             (let ((hide-todo-edit-elements (ps:chain document (get-elements-by-class-name hide-todo-edit-class-name)))
                                   (show-todo-edit-elements (ps:chain document (get-elements-by-class-name show-todo-edit-class-name)))
                                   (todo-text-element (ps:chain document (get-element-by-id todo-text-id))))
                               (dolist (e hide-todo-edit-elements) (setf (@ e hidden) show-edit))
                               (dolist (e show-todo-edit-elements) (setf (@ e hidden) (not show-edit)))
                               (setf (@ todo-text-element value) (@ todo text))
                               (ps:chain todo-text-element (focus)))
                             t)
                           (save-input-for (todo)
                             (let* ((todo-text-element (ps:chain document (get-element-by-id todo-text-id)))
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
                    (flet ((update-checked-count ()
                             (let ((checked-count (length (ps:chain document (query-selector-all "label pre[style*=line-through]")))))
                               (setf (ps:chain column-header inner-text)
                                   (+ (if use-plural-form "To-do Items" "To-do Item") " " (ps:chain (+ checked-count) (to-string)) "/" (ps:chain count (to-string)))))))
                      (jfh-web::with-html-elements
                          (tr
                           (td
                            (input
                             (id . "(progn todo-checkbox-id)")
                             (type . "checkbox")
                             (onclick . "(update-todo (progn index) (@ todo id)))")
                             (onclick . "(update-checked-count)")
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
                            (button (hidden . "t") (onclick . "(delete-todo todo)") (class . "(progn show-todo-edit-class-name)") "Delete"))))))
                  t))))))

(defun client-ui-recipe ()
  "define client side UI functions"
  (ps:ps

    (defparameter *recipe-list* "recipe-list")
    (defparameter *recipe-entry* "recipe-entry")
    (defparameter *recipe-details* "recipe-details")
    ;; (defparameter *todo-label* "todo-label")
    ;; (defparameter *show-todo-edit* "show-todo-edit")
    ;; (defparameter *hide-todo-edit* "hide-todo-edit")
    ;; (defparameter *todo-text* "todo-text")
    
    (setf (ps:chain window onload) init-recipe)))

(define-for-ps init-recipe ()
  "initialize html elements and JS objects on page load"
  (with-callback
      (get-recipe-list-from-server)
    ;; (setf add-button (ps:chain document
    ;;                         (get-element-by-id "todo-add-btn")))
    ;; (ps:chain add-button
    ;;        (add-event-listener "click" add-todo false))
    (render-recipe-menu)
    (render-recipe-list)))

(define-for-ps get-recipe-sections ()
  (let ((list-section (ps:chain document (get-element-by-id *recipe-list*)))
        (add-section (ps:chain document (get-element-by-id *recipe-entry*)))
        (detail-section (ps:chain document (get-element-by-id *recipe-details*))))
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
  (let* ((recipe-list-entries (ps:chain document (get-element-by-id "recipe-list-entries")))
         (parent-element recipe-list-entries))
    (clear-children parent-element)
    (ps:chain recipe-list
           (map
            #'(lambda (recipe)
                (let ((recipe-id (@ recipe id)))
                  (jfh-web::with-html-elements
                      (p
                       (p (a (onclick . "(render-recipe-detail recipe-id)") "(@ recipe name)")))))))))
  t)

(define-for-ps render-recipe-add ()
  (display-recipe-section *recipe-entry*)
  (let* ((recipe-entry-fields (ps:chain document (get-element-by-id "recipe-entry-fields")))
         (parent-element recipe-entry-fields))
    (clear-children parent-element)
    (jfh-web::with-html-elements
        (p
         (p
          (input (type . "text") (id . "recipe-name") (placeholder . "Recipe Name")))
         (p
          (textarea (id . "recipe-ingredients-entry") (placeholder . "Ingredients") (rows . "10") (cols . "100")))
         (p
          (textarea (id . "recipe-steps-entry") (placeholder . "Steps") (rows . "10") (cols . "100")))
         (p
          (button (onclick . "(add-recipe)") "Add Recipe")))))
  t)

(define-for-ps get-recipe-by-id (recipe-id)
  (let ((recipe-index (ps:chain recipe-list (find-index #'(lambda (recipe) (= recipe-id (@ recipe id)))))))
    (aref recipe-list recipe-index)))

(define-for-ps render-recipe-detail (recipe-id)
  (display-recipe-section *recipe-details*)
  (flet ((display-name ()
           (let* ((recipe-name (ps:chain document (get-element-by-id "recipe-detail-name")))
                  (parent-element recipe-name)
                  (name (@ (get-recipe-by-id recipe-id) :name)))
             (clear-children parent-element)
             (jfh-web::with-html-elements
                 (div
                  (h2 name)))))
         (display-recipe-items (list-type)
           (let* ((recipe-items (ps:chain document (get-element-by-id (+ "recipe-" list-type))))
                  (parent-element recipe-items))
             (clear-children parent-element)
             (ps:chain (getprop (get-recipe-by-id recipe-id) list-type)
                    (map
                     #'(lambda (item index)
                         (let ((checkbox-id (+ list-type "-" index)))
                           (jfh-web::with-html-elements
                               (p
                                (input (id . "(progn checkbox-id)") (type . "checkbox"))
                                (label (for . "(progn checkbox-id)") item))))))))))
    (display-name)
    (display-recipe-items :ingredients)
    (display-recipe-items :steps))
  t)

(define-for-ps render-recipe-menu ()
  (let* ((recipe-menu (ps:chain document (get-element-by-id "recipe-menu")))
         (parent-element recipe-menu))
    (jfh-web::with-html-elements
        (tr
         (td (span (a (onclick . "(render-recipe-list)") "List")))
         (td #\Space #\Space #\Space)
         (td (span (a (onclick . "(render-recipe-add)") "Add")))))))
