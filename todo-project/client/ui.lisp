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
    (defparameter *todos-filtered-by-tags* (list))
    (defparameter *selected-filter-tag-todo-ids* (list))
    (defparameter *max-candidate-tag-show-count* 10)
    (defparameter *filter-tag-match-type* 'any)

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
    (with-callback
        (get-tag-list-from-server)
      (get-todo-list-from-server)
      (render-tag-filter)
      (get-tag-todo-associaton-list-from-server)
      (set-filter-tag-match-type *filter-tag-match-type*)
      t)
    t)
  (let ((add-button (ps:chain document (get-element-by-id "todo-add-btn")))
        (todo-content (ps:chain document (get-element-by-id "todo-content"))))
    (ps:chain add-button (add-event-listener "click" add-todo false))
    (ps:chain todo-content (add-event-listener "input" render-tag-content-for-new-todo todo-content event false))))

(define-for-ps render-app-settings ()
  "render html elements for app settings"
  (setf *selected-filter-tag-todo-ids* (@ *app-settings* selected-filter-tag-todo-ids))
  (setf *filter-tag-match-type* (@ *app-settings* filter-tag-match-type))
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
         (div (id . "filter-tag-candidate-area")
              (span "Choose tag to filter on")
              (span (style . "margin-left:30px;") "Match on: ")
              (span (id . "match-any") (style . "color:red; margin-left:10px; margin-right:15px; text-decoration:underline;") (onclick . "(set-filter-tag-match-type 'any)") "ANY")
              (span "|")
              (span (id . "match-all") (style . "margin-left:15px; margin-right:15px; text-decoration:underline;") (onclick . "(set-filter-tag-match-type 'all)") "ALL")
              (br (ref . "br"))
              (div (id . "filter-tag-candidates"))
              (div (id . "filter-tag-candidates-selected")))
         (div
          (span (br (ref . "br")))
          (input (id . "todo-filter-text") (type . "textbox") (placeholder . "Enter text to filter on here") (value . "(@ *app-settings* filter-text)")))
         (span "  ")
         (button (onclick . "(filter-todos)") "Filter"))))
  t)

(define-for-ps set-filter-tag-match-type (filter-tag-match-type)
  
  (setf *filter-tag-match-type* filter-tag-match-type)
  
  (flet ((set-color (element-id color)
           (setf (ps:@ (ps:@ (ps:chain document (get-element-by-id element-id)) style) color) color)))
    (cond
      ((= 'any filter-tag-match-type)
       (set-color "match-any" 'red)
       (set-color "match-all" ""))
      ((= 'all filter-tag-match-type)
       (set-color "match-all" 'red)
       (set-color "match-any" ""))))
  (render-filter-tag-todos "filter-"))

(define-for-ps render-todos-filtered-by-tags (filter-todo-ids)
  "Filter the actual todos with the given list of todo IDs."
  (let ((filtered-todos (ps:chain todo-list (filter (lambda (todo) (>= (ps:chain filter-todo-ids (find-index #'(lambda (todo-id) (= todo-id (ps:chain (= (@ todo id))))))) 0))))))
    (render-todo-list filtered-todos)
    (update-app-settings :can-re-render f))
  t)

(define-for-ps get-filter-todo-ids (todo-ids tag-ids selected-filter-tag-todo-ids)
  (labels ((matching-tag-todos (tag-todo tag-id todo-id)
             (and
              (= todo-id (@ tag-todo todo-id))
              (= tag-id (@ tag-todo tag-id))))
           (matching-tag-by-todo-id (tag-id todo-id)
             (>
              (ps:chain selected-filter-tag-todo-ids
                        (find-index #'(lambda (tag-todo) (matching-tag-todos tag-todo tag-id todo-id))))
              0))
           (get-todos-that-match-all-selected-tags (todo-id)
             (when (ps:chain tag-ids (every #'(lambda (tag-id) (matching-tag-by-todo-id tag-id todo-id))))
               todo-id)))
    (cond
      ((= 'any *filter-tag-match-type*)
       todo-ids)
      ((= 'all *filter-tag-match-type*)
       (ps:chain
        (ps:chain todo-ids
                  (map #'(lambda (todo-id) (get-todos-that-match-all-selected-tags todo-id))))
        (filter #'(lambda (todo-id) todo-id)))))))

(define-for-ps search-for-tag (tag candidate-tag-id-prefix)
  (let* ((tag-id (ps:@ tag id))
         (tag-list-element-id (+ candidate-tag-id-prefix *candidate-tag-text* tag-id))
         (selected-tags (ps:chain *tags-todo-association-list* (filter #'(lambda (tag-todo) (= (ps:@ tag id) (ps:@ tag-todo tag-id)))))))
    (ps:chain selected-tags (for-each #'(lambda (tag-todo) (ps:chain *selected-filter-tag-todo-ids* (push tag-todo)))))
    (ps:chain (ps:chain document (get-element-by-id tag-list-element-id)) (remove))
    ;; (add-associate-tag-to-todo (ps:create tag-id tag-id todo-id todo-id))
    (render-filter-tag-todos candidate-tag-id-prefix))
  t)

(define-for-ps render-filter-tag-todos (candidate-tag-id-prefix)
  "Render selected tags and todos."
  (let ((filter-tags (ps:chain *selected-filter-tag-todo-ids* (map #'(lambda (tag-todo) (ps:@ tag-todo tag-id))) (filter #'(lambda (tag-id index tag-ids) (= (ps:chain tag-ids (find-index #'(lambda (id) (= id tag-id)))) index)))))
        (filter-todo-ids (ps:chain *selected-filter-tag-todo-ids* (map #'(lambda (tag-todo) (ps:@ tag-todo todo-id))))))
    (render-selected-tags filter-tags candidate-tag-id-prefix)
    (setf *todos-filtered-by-tags* (get-filter-todo-ids filter-todo-ids filter-tags *selected-filter-tag-todo-ids*))
    (render-todos-filtered-by-tags *todos-filtered-by-tags*))
  ;; (ps:chain console (log (ps:chain *tags-todo-association-list* (filter #'(lambda (tag-todo) (= (ps:@ tag id) (ps:@ tag-todo tag-id)))))))
  t)

(define-for-ps render-tag-filter ()
  (let* ((filter-tag-candidates (ps:chain document (get-element-by-id "filter-tag-candidates")))
         (candidate-tag-id-prefix "filter-")
         (parent-element (ps:chain document (get-element-by-id (+ (ps:@ filter-tag-candidates id) "-selected")))))
    (render-tag-candidates (ps:chain *tag-list*) filter-tag-candidates candidate-tag-id-prefix #'search-for-tag)
    (jfh-web::with-html-elements
        (div (id . "(+ candidate-tag-id-prefix \"selected-tags\")") (class . "tag-display"))))
  t)

(define-for-ps get-tag-content-area-element (id-prefix)
  "get the element that contains the tag area"
  (ps:chain document (get-element-by-id (+ id-prefix "tag-content"))))

(define-for-ps render-selected-tags (selected-tag-ids &optional (candidate-tag-id-prefix ""))
  "render the tags selected to go with the current todo item"
  (let* ((parent-element (ps:chain document (get-element-by-id (+ candidate-tag-id-prefix "selected-tags"))))
         (selected-tags (ps:chain selected-tag-ids (map #'(lambda (selected-tag-id) (ps:chain *taglist* (find #'(lambda (tag) (= (ps:@ tag id) selected-tag-id))))))))
         (selected-tags-element parent-element)
         (import-selected-tags (ps:chain document (get-element-by-id "import-selected-tags"))))
    (when selected-tags-element
      (clear-children parent-element))
    (when import-selected-tags
      (setf (ps:@ import-selected-tags value) selected-tag-ids))
    (ps:chain selected-tags
              (map
               #'(lambda (tag)
                   (let ((tag-id (+ *selected-tag-text* (ps:@ tag id))))
                     (jfh-web::with-html-elements
                         (span
                          (style . "margin: 5px;")
                          (a (id . "(progn tag-id)") (onclick . "(remove-tag-from-todo tag candidate-tag-id-prefix)") "(ps:@ tag text)"))))
                   t))))
  t)

(define-for-ps add-tag-to-todo (tag id-prefix)
  (let* ((tag-id (ps:@ tag id))
         (tag-list-element-id (+ id-prefix *candidate-tag-text* tag-id)))
    (ps:chain *selected-tag-ids* (push tag-id))
    (ps:chain (ps:chain document (get-element-by-id tag-list-element-id)) (remove))
    (render-selected-tags *selected-tag-ids* id-prefix))
  t)

(define-for-ps remove-tag-from-todo (tag id-prefix)
  (let* ((tag-id (ps:@ tag id))
         (tag-list-element-id (+ *selected-tag-text* tag-id)))
    (ps:chain (ps:chain document (get-element-by-id tag-list-element-id)) (remove))
    (display-candidate-tag tag (ps:chain document (get-element-by-id (+ id-prefix "tag-candidates"))) id-prefix #'search-for-tag)
    (let ((remove-tag-index (ps:chain *selected-tag-ids* (find-index #'(lambda (selected-tag-id) (= tag-id selected-tag-id))))))
      (when (>= remove-tag-index 0)
        (ps:chain *selected-tag-ids* (splice remove-tag-index 1))))
    (when (= "filter-" id-prefix)
      (setf *selected-filter-tag-todo-ids* (ps:chain *selected-filter-tag-todo-ids* (filter #'(lambda (selected-tag-todo) (not (= tag-id (ps:@ selected-tag-todo tag-id)))))))
      (render-filter-tag-todos id-prefix)))
  t)

(define-for-ps display-candidate-tag (candidate-tag parent-element &optional (candidate-tag-id-prefix "") (tag-click-handler #'add-tag-to-todo))
  "displays 1 candidate tag"
  (let ((candidate-tag-id (+ candidate-tag-id-prefix *candidate-tag-text* (ps:@ candidate-tag id))))
    (jfh-web::with-html-elements
        (span
         (style . "margin: 5px;")
         (a (id . "(progn candidate-tag-id)") (onclick . "(tag-click-handler candidate-tag candidate-tag-id-prefix)") "(ps:@ candidate-tag text)"))))
  t)

(define-for-ps render-tag-candidates (candidate-tags parent-element &optional (candidate-tag-id-prefix "") (tag-click-handler #'add-tag-to-todo))
  (ps:chain candidate-tags
            (map #'(lambda (candidate-tag) (display-candidate-tag candidate-tag parent-element candidate-tag-id-prefix tag-click-handler) t)))
  t)

(var *show-tag-content-handler*)

(define-for-ps render-tag-content-for-new-todo (input event)
  (render-tag-content input "new-todo-"))

(define-for-ps render-tag-content-for-edit-todo (todo-id index)
  (render-tag-content todo-id (+ "edit-todo-" index "-")))

(define-for-ps render-tag-content (todo-id id-prefix)
  "render tag entry, selected tags, and tag candidates"
  (let ((tag-content-area (get-tag-content-area-element id-prefix))
        (tag-candidate-area (ps:chain document (get-element-by-id (+ id-prefix "tag-candidates")))))
    (labels ((search-tags (event)
               (clear-children (ps:chain document (get-element-by-id (+ id-prefix "tag-candidates"))))
               (let* ((search-input (ps:chain (ps:@ (ps:chain document (get-element-by-id (+ id-prefix "tag-input"))) value) (to-lower-case)))
                      (search-results (ps:chain *tag-list*
                                                (filter #'(lambda (tag) (>= (ps:chain (ps:chain (ps:@ tag text) (to-lower-case)) (index-of search-input)) 0))))))
                 (render-tag-candidates search-results tag-candidate-area id-prefix))
               t))
      (flet ((tag-content-visible ()
               (not (ps:@ tag-content-area hidden)))
             (render-tag-entry ()
               (let ((parent-element tag-content-area))
                 (jfh-web::with-html-elements
                     (div (input (type . "text") (id . "(+ id-prefix \"tag-input\")") (placeholder . "add new tag") (oninput . "(search-tags)"))
                          (button (style . "margin-left: 30px;") (onclick . "(add-tag id-prefix)") "Add Tag")
                          (div (id . "(+ id-prefix \"selected-tags\")") (class . "tag-display")))))
               t)
             (show-tag-content-area (show)
               (let ((hide (not show)))
                 (setf (ps:@ tag-content-area hidden) hide)))
             (tag-elements-already-exist ()
                (ps:chain document (get-element-by-id (+ id-prefix "tag-input")))))
        (unless (tag-content-visible)
          (show-tag-content-area t)
          (setf *show-tag-content-handler* #'(lambda () (show-tag-content-area ps:f)))
          (unless (tag-elements-already-exist)
            (render-tag-candidates (ps:chain *tag-list* (slice 0 *max-candidate-tag-show-count*)) tag-candidate-area id-prefix)
            (render-tag-entry)
            (when todo-id
              (let ((tags-for-this-todo (ps:chain *tags-todo-association-list* (filter #'(lambda (tag-todo) (= (ps:@ tag-todo todo-id) todo-id))) (map #'(lambda (tag-todo) (ps:@ tag-todo tag-id))))))
                (render-selected-tags tags-for-this-todo id-prefix)
                (setf *selected-tag-ids* tags-for-this-todo))))))))
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
                                                   (or
                                                    (ps:chain (@ todo text) (match (new (-reg-exp filter-text "i"))))
                                                    (ps:chain *todos-filtered-by-tags* (some #'(lambda (filtered-todo-id) (= filtered-todo-id (@ todo id)))))))))))
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
                         (tag-content-id (+ "edit-todo-" index "-tag-content"))
                         (tag-candidates-id (+ "edit-todo-" index "-tag-candidates"))
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
                                (render-tag-content-for-edit-todo (@ todo id) index)
                                t)
                              (save-input-for (todo)
                                (let* ((todo-text-element (ps:chain document (get-element-by-id todo-text-id)))
                                       (updated-text (@ todo-text-element value)))
                                  (setf (@ todo text) updated-text)
                                  (update-todo-from-edit todo)
                                  (show-input-for todo false)
                                  (funcall *show-tag-content-handler*))
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
                               (div (id . "(progn tag-content-id)") (hidden . "true")
                                    (div (id . "(progn tag-candidates-id)") (class . "tag-display")))
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

(defun client-import ()
  "Define client side functions to handle todo imports."
  (ps:ps
   ;; (defvar todo-list ([]))
   ;; (defparameter *tag-list* ([]))
   (defparameter *tags-todo-association-list* ([]))))

(defun client-ui-import ()
  "define client side UI functions"
  (ps:ps
    (defparameter *candidate-tag-text* "candidate-tag")
    (defparameter *selected-tag-text* "selected-tag")

    (defparameter *selected-tag-ids* (list))
    ;; (defparameter *selected-filter-tag-todo-ids* (list))
    (defparameter *max-candidate-tag-show-count* 1000)
    ;; (defparameter *filter-tag-match-type* 'any)

    (setf (ps:chain window onload) init-import)))

(define-for-ps init-import ()
  "initialize html elements and JS objects on page load"
  (with-callback
    ;;   (get-app-settings-from-server)
    ;; (get-todo-list-from-server)
    (get-tag-list-from-server)
    ;; (get-tag-todo-associaton-list-from-server)
    )
  (let ((todo-content (ps:chain document (get-element-by-id "import-list"))))
    ;; (ps:chain add-button (add-event-listener "click" add-todo false))
    (ps:chain todo-content (add-event-listener "input" render-tag-content-for-import-todo todo-content event false))))


(define-for-ps render-tag-content-for-import-todo (input event)
  (render-tag-content input "import-todo-"))
