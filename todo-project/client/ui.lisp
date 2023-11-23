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

    (defparameter *candidate-tag-ids-for-current-todo* (make-candidate-tag-ids-for-current-todo (list)))
    (defparameter *selected-tag-ids-for-current-todo* (make-selected-tag-ids-for-current-todo (list)))
    (defparameter *selected-filter-tag-ids* (make-selected-filter-tag-ids (list)))
    ;; (defparameter *max-candidate-tag-show-count* 10)
    (defparameter *filter-tag-match-type* 'any)

    (setf (ps:chain window onload) init)))

;;(ps:ps
;; (defmacro
(defpsmacro with-callback (fn &body body)
   `(,(car fn) ,@(cdr fn) #'(lambda (),@body)));;)

;; (ps:ps
;; (defmacro
(defpsmacro with-chained-callback (&body body)
   (labels ((make-with-callback (fn body)
              (cond
                ((null body)
                 `(progn
                    (,fn)
                    t))
                (t `(with-callback ,fn ,(make-with-callback (car body) (cdr body)))))))
     (let ((fn (car body)))
       (make-with-callback fn (cdr body))))) ;;)

(define-for-ps clear-field (field)
  "clear input field's value"
  (setf (ps:chain field value) "")
  t)
    
(define-for-ps clear-children (parent-element)
  "remove all child nodes of a parent element"
  (ps:loop
     while (ps:chain parent-element (has-child-nodes))
     do
	(ps:chain parent-element (remove-child (@ parent-element first-child)))))

(define-for-ps init-event-handlers ()
  "Initialize html elements on page load; add event handlers"
  (let ((add-button (ps:chain document (get-element-by-id "todo-add-btn")))
        (todo-content (ps:chain document (get-element-by-id "todo-content"))))
    (ps:chain add-button (add-event-listener "click" add-todo false))
    (ps:chain todo-content (add-event-listener "input" render-tag-content-for-new-todo event false))))

(define-for-ps init ()
  "Initialize JS objects on page load; get todos and tags from the server then render them on the client"
  (init-event-handlers)

  (with-chained-callback
      (get-app-settings-from-server)
    (get-tag-list-from-server)
    (get-todo-list-from-server)
    (get-tag-todo-associaton-list-from-server)
    (get-tag-mru-list-from-server)
    ;; NOTE: I need to wrap the following 2 procedural function calls inside a lambda so they both are invoked as part of the previous function's callback
    #'(lambda () 
        (render-tag-filter)
	(render-tag-entry "filter-")
        (render-tag-summary "filter-")
        (set-filter-tag-match-type-and-re-render-filter (get-filter-tag-match-type))
	(render-tag-area "new-todo-")
        t)))

(define-for-ps render-app-settings ()
  "render html elements for app settings, including for tags selected for todo filter"
  (let ((app-settings (app-settings 'get-app-settings)))
    (init-selected-filter-tag-ids (@ app-settings selected-filter-tag-ids))
    (setf *filter-tag-match-type* (@ app-settings filter-tag-match-type))
    (let ((parent-element (ps:chain document (get-element-by-id "app-settings"))))
      (jfh-web::with-html-elements
          (div
           (input (id . "hide-done") (type . "checkbox") (onclick . "(update-app-settings)") (checked . "(@ app-settings hide-done-items)"))
           (label (for . "hide-done") "Hide Done Items."))))))

(define-for-ps filter-todos ()
  (render-filter-tag-todos "filter-")
  (update-app-settings)
  t)

(define-for-ps render-todo-filter ()
  "render html elements for todo filter"
  (let ((parent-element (ps:chain document (get-element-by-id "todo-filter"))))
    (jfh-web::with-html-elements
        (div
         (div
          (span (br (ref . "br")))
          (input (id . "todo-filter-text") (type . "textbox") (placeholder . "Enter text to filter on here") (value . "(@ (app-settings 'get-app-settings) filter-text)")))
         (span "  ")
         (button (onclick . "(filter-todos)") "Filter"))))
  t)

(define-for-ps render-tag-filter-ui-by-area-independent (id-prefix)
  (let ((parent-element ;; (ps:chain document (get-element-by-id (+ id-prefix "tag-content")))
	  (ps:chain document (get-element-by-id "edit-todo-tag-content"))
			))
    (jfh-web::with-html-elements
	(div
	 (div (id . "(+ id-prefix \"tag-candidate-area\")")
	      (div (id . "(+ id-prefix \"tag-candidates-selected\")"))
	      (div (id . "(+ id-prefix \"tag-candidates\")") (style . "border-style:solid;border-color:green;padding:5px;margin-top: 5px;")))))))

(define-for-ps render-tag-filter-ui ()
  "render html elements for tag filter - this is the new one"
  (flet ((render-tag-filter-ui-by-area (id-prefix)
	   (let ((parent-element (ps:chain document (get-element-by-id (+ id-prefix "tag-content")))))
	     (jfh-web::with-html-elements
		 (div
		  (div (id . "(+ id-prefix \"tag-candidate-area\")")
		       (div (id . "(+ id-prefix \"tag-candidates-selected\")"))
		       (div (id . "(+ id-prefix \"tag-candidates\")") (style . "border-style:solid;border-color:green;padding:5px;margin-top: 5px;"))))))))
    (render-tag-filter-ui-by-area "filter-")
    (render-tag-filter-ui-by-area "new-todo-")
    (render-tag-filter-ui-by-area "edit-todo-"))
  t)

(define-for-ps render-tag-entry (id-prefix)
  "Independent tag entry and search"
  (let ((tag-candidate-area (ps:chain document (get-element-by-id (+ id-prefix "tag-candidates")))))
    (flet ((search-tags (event)
	     (let* ((search-input (ps:@ (ps:chain document (get-element-by-id (+ id-prefix "tag-input"))) value))
		    (search-results (get-tags-matching-search-input (get-all-tags) search-input)))
	       (render-tag-candidates search-results tag-candidate-area id-prefix #'move-tag-from-candidate-to-selected t))
	     t))
      (let ((parent-element (get-tag-content-area-element id-prefix)))
	(jfh-web::with-html-elements
            (div
             (div (class . "tag-display") (style . "border-color: green; border-style: solid;")
		  (input (type . "text") (id . "(+ id-prefix \"tag-input\")") (placeholder . "add new tag") (oninput . "(search-tags)") (style . "margin-left: 5px;"))
		  (button (style . "margin-left: 30px;") (onclick . "(add-tag id-prefix)") "Add Tag"))
             ;; (div (id . "(+ id-prefix \"selected-tags\")") (class . "tag-display") (style . "border-color: green; border-style: solid;"))
             )))))
  t)

(define-for-ps get-filter-tag-match-type ()
  "Get filter tag match type"
  *filter-tag-match-type*)

(define-for-ps render-filter-tag-match-type (filter-tag-match-type)
  "Render the part of ther UI related to the filter tag match type - ANY or ALL"
  (flet ((set-color (element-id color)
           (setf (ps:@ (ps:@ (ps:chain document (get-element-by-id element-id)) style) color) color)))
    (cond
      ((= 'any filter-tag-match-type)
       (set-color "match-any" 'red)
       (set-color "match-all" ""))
      ((= 'all filter-tag-match-type)
       (set-color "match-all" 'red)
       (set-color "match-any" "")))))

(define-for-ps set-filter-tag-match-type-and-re-render-filter (filter-tag-match-type)
  "Set filter tag match type to any or all."
  (setf *filter-tag-match-type* filter-tag-match-type)

  (render-filter-tag-match-type filter-tag-match-type)

  (render-filter-tag-todos "filter-"))

(define-for-ps render-todos-filtered-by-tags (filter-todo-ids)
  "Filter the actual todos with the given list of todo IDs. The todo item list is filtered by tags."
  (let ((filtered-todos (remove-if-not* (lambda (todo) (>= (position-if* #'(lambda (todo-id) (= todo-id (@ todo id))) filter-todo-ids) 0)) (get-all-todos))))
    (render-todo-list filtered-todos)
    (update-app-settings :can-re-render f))
  t)

(define-for-ps get-filter-todo-ids (todo-ids tag-ids selected-filter-tag-todo-ids filter-tag-match-type)
  "Get todo items filtered by currently selected tag IDs (page level). 'ANY just returns all todo items; 'ALL returns todo items that match all the currently selected tags."
  (labels ((matching-tag-todos (tag-todo tag-id todo-id)
             (and
              (= todo-id (@ tag-todo todo-id))
              (= tag-id (@ tag-todo tag-id))))
           (matching-tag-by-todo-id (tag-id todo-id)
             (>=
              (position-if* #'(lambda (tag-todo) (matching-tag-todos tag-todo tag-id todo-id)) selected-filter-tag-todo-ids)
              0))
           (get-todos-that-match-all-selected-tags (todo-id)
             (when (> (length (remove-if-not* #'(lambda (tag-id) (matching-tag-by-todo-id tag-id todo-id)) tag-ids)) 0)
               todo-id)))
    (case filter-tag-match-type
      (any todo-ids)
      (all
       (if (length selected-filter-tag-todo-ids)
           (get-todos-matching-all-selected-tags todo-ids selected-filter-tag-todo-ids tag-ids)
           todo-ids)))))

(define-for-ps search-for-tag (tag candidate-tag-id-prefix)
  "Click handler for candidate tags in global filter; moves tag from candiates to selected and filters and re-renders todo list"
  (let ((tag-id (ps:@ tag id)))
    (add-selected-tag-id-to-selected-filter-tag-ids tag-id)
    (move-tag-from-candidate-to-selected tag candidate-tag-id-prefix)
    (render-filter-tag-todos candidate-tag-id-prefix))
  t)

(define-for-ps render-filter-tag-todos (candidate-tag-id-prefix)
  "Render selected tags and todos."
  (let* ((selected-tag-ids (get-currently-selected-tag-ids candidate-tag-id-prefix))
         (tag-todos (get-all-tag-todos))
         (tag-todo-matches-selection (lambda (tag-todo) (>= (position-if* (lambda (selected-tag-id) (= selected-tag-id (ps:@ tag-todo tag-id))) selected-tag-ids) 0)))
         (matching-tag-todos (if (length selected-tag-ids) (remove-if-not* #'tag-todo-matches-selection tag-todos) tag-todos))
         (matching-todo-ids (if (length selected-tag-ids) (map* #'(lambda (tag-todo) (ps:@ tag-todo todo-id)) matching-tag-todos) (map* (lambda (todo) (ps:@ todo id)) (get-all-todos)))))
    (render-todos-filtered-by-tags (get-filter-todo-ids matching-todo-ids selected-tag-ids matching-tag-todos (get-filter-tag-match-type))))
  t)

(define-for-ps get-todos-matching-all-selected-tags (matching-todo-ids matching-tag-todos selected-tag-ids)
  (labels ((identity (e)
             (if e ps:t ps:f))
           (flag-each-tag-id-if-matches-selected-tag-id (todo-with-flat-tag-ids selected-tag-id)
             (>= (position-if* (lambda (tag-id) (= selected-tag-id tag-id)) (ps:@ todo-with-flat-tag-ids tag-ids)) 0)))
    (flet ((get-todos-with-flattened-tag-ids (todo-ids tag-todos)
             (map*
              (lambda (todo-id)
                (create todo-id todo-id
                        tag-ids (map*
                                 (lambda (tag-todo) (ps:@ tag-todo tag-id))
                                 (remove-if-not*
                                  (lambda (tag-todo) (= todo-id (ps:@ tag-todo todo-id)))
                                  tag-todos))))
              todo-ids))
           (all-tag-ids-match-per-todo (todo+selected-tag-flag)
             (ps:@ todo+selected-tag-flag all-tags-match))
           (each-todo-with-all-tags-match-flag (todo+selected-tag-flags)
             (create todo-id (ps:@ todo+selected-tag-flags todo-id)
                     all-tags-match (every* #'identity (ps:@ todo+selected-tag-flags flags))))
           (each-todo-with-tag-id-flags (todo-with-flat-tag-ids)
             (create
              todo-id (ps:@ todo-with-flat-tag-ids todo-id)
              flags
              (map*
               (lambda (selected-tag-id) (flag-each-tag-id-if-matches-selected-tag-id todo-with-flat-tag-ids selected-tag-id))
               selected-tag-ids)))
           (get-todo-ids (todo-matching-all-select-ids) (ps:@ todo-matching-all-select-ids todo-id)))
      (let ((todos-with-flattened-tag-ids (get-todos-with-flattened-tag-ids matching-todo-ids matching-tag-todos)))
        (map* #'get-todo-ids
              (remove-if-not* #'all-tag-ids-match-per-todo
                              (map* #'each-todo-with-all-tags-match-flag ;; are all the selected tags in the tag ID list for this todo?
                                    (map* #'each-todo-with-tag-id-flags todos-with-flattened-tag-ids))))))))

(define-for-ps render-tag-summary (id-prefix)
  "Renders page level tag filter summary."
  (labels ((^= (string sub-string)
	     (ps:chain string (starts-with sub-string))))
    (flet ((get-parent-element ()
	     (cond
	       ((= "filter-" id-prefix) (ps:chain document (get-element-by-id "todo-filter")))
	       ((= "new-todo-" id-prefix) (ps:chain document (get-element-by-id "todo-new-entry-and-import")))
	       ((^= id-prefix "edit-todo-") (ps:@ (ps:chain document (query-selector (+ "div[id^='" id-prefix "']"))) parent-element))))
	   (tag-content-visible ()
             (and
	      tag-summary-area
	      (> (length (ps:@ tag-summary-area id)) 0))))
      (let* ((filter-tag-candidates-area (ps:chain document (get-element-by-id (+ id-prefix "tag-candidates"))))
             (summary-tag-id-prefix (+ id-prefix "summary-"))
             (parent-element (get-parent-element))
	     (tag-summary-area (ps:chain document (get-element-by-id (+ summary-tag-id-prefix "selected-tags")))))

	(when (tag-content-visible)
	  (clear-children (ps:chain document (get-element-by-id (+ summary-tag-id-prefix "selected-tags")))))
	(unless (tag-content-visible)
	  (jfh-web::with-html-elements
              (div (id . "(+ summary-tag-id-prefix \"selected-tags\")") (class . "tag-display") (style . "border-color: green;border-style:solid;") (onclick . "(show-tag-area id-prefix t)"))))
	(let ((selected-tag-ids (if (= "filter-" id-prefix) (get-selected-filter-tag-ids) (get-currently-selected-tag-ids id-prefix)))) ;; TODO: change here next
	  (render-selected-tags-summary selected-tag-ids summary-tag-id-prefix)))
      t)))

(define-for-ps render-tag-filter ()
  "Renders page level tag filter."
  (let* ((filter-tag-candidates-area (ps:chain document (get-element-by-id "filter-tag-candidates")))
         (candidate-tag-id-prefix "filter-")
         (parent-element (ps:chain document (get-element-by-id (+ (ps:@ filter-tag-candidates-area id) "-selected")))))
    (render-tag-candidates (get-all-tags) filter-tag-candidates-area candidate-tag-id-prefix #'search-for-tag)
    (jfh-web::with-html-elements
        (div (id . "(+ candidate-tag-id-prefix \"selected-tags\")") (class . "tag-display") (style . "border-color: green;border-style:solid;")))
    (render-selected-tags (get-currently-selected-tag-ids candidate-tag-id-prefix) candidate-tag-id-prefix))
  t)

;; (if todo-id (ps:chain document (get-element-by-id "edit-todo-tag-content")) (ps:chain document (get-element-by-id (+ id-prefix "tag-candidates"))))
;; (if todo-id filter-tag-candidates-area (ps:chain document (get-element-by-id (+ (ps:@ filter-tag-candidates-area id) "-selected"))))
(define-for-ps render-tag-area (id-prefix &optional todo-id)
  "Renders tag area."
  (let* ((filter-tag-candidates-area (ps:chain document (get-element-by-id (+ id-prefix "tag-candidates"))))
         (parent-element (ps:chain document (get-element-by-id (+ (ps:@ filter-tag-candidates-area id) "-selected")))))
    (render-tag-candidates (get-all-tags) filter-tag-candidates-area id-prefix #'search-for-tag)
    (jfh-web::with-html-elements
        (div (id . "(+ id-prefix \"selected-tags\")") (class . "tag-display") (style . "border-color: green;border-style:solid;")))
    (render-selected-tags (get-currently-selected-tag-ids id-prefix) id-prefix))
  t)

(define-for-ps get-tag-content-area-element (id-prefix)
  "get the element that contains the tag area"
  (ps:chain document (get-element-by-id (+ id-prefix "tag-content"))))

(define-for-ps show-tag-area (id-prefix show-tags)
  (setf (ps:@ (ps:chain document (get-element-by-id "todo-list-area")) hidden) show-tags)
  (setf (ps:@ (ps:chain document (get-element-by-id "todo-filter")) hidden) show-tags)
  (setf (ps:@ (ps:chain document (get-element-by-id "todo-new-entry-and-import")) hidden) show-tags)
  (setf (ps:@ (ps:chain document (get-element-by-id "edit-todo-tag-content")) hidden) (not show-tags))
  (setf (ps:@ (ps:chain document (get-element-by-id (+ id-prefix "tag-content"))) hidden) (not show-tags))
  (ps:chain window (scroll-To 0 0)))

(define-for-ps render-selected-tags-summary (selected-tag-ids &optional (candidate-tag-id-prefix ""))
  "render the tags selected to go with the current todo item"
  (flet ((get-select-tags-caption ()
	   (if (>= (ps:chain candidate-tag-id-prefix (index-of "todo")) 0)
	       (if (zerop* (length selected-tags)) "Add Tags to this item..." "Selected Tags for this Item")
	       (if (zerop* (length selected-tags)) "Add Tags to Filter..." "Selected Tags"))))
    (let* ((parent-element (ps:chain document (get-element-by-id (+ candidate-tag-id-prefix "selected-tags"))))
           (selected-tags (map* #'(lambda (selected-tag-id) (find-if* #'(lambda (tag) (= (ps:@ tag id) selected-tag-id)) (get-all-tags))) selected-tag-ids))
           (selected-tags-element parent-element)
           (import-selected-tags (ps:chain document (get-element-by-id "import-selected-tags"))))
      (when selected-tags-element
	(clear-children parent-element)
	(cond
          ((= "import-todo-" candidate-tag-id-prefix)
           (jfh-web::with-html-elements
               (div (div (style . "margin: 5px;") "Tags to associate with imported To-Do Items:"))))
          (t
           (let ((select-tags-caption (get-select-tags-caption)))
	     (jfh-web::with-html-elements
		 (div (style . "margin: 5px;") (progn select-tags-caption)))))))
      (when import-selected-tags
	(setf (ps:@ import-selected-tags value) selected-tag-ids))
      (map*
       #'(lambda (tag)
           (let ((tag-id (+ candidate-tag-id-prefix *selected-tag-text* (ps:@ tag id))))
             (jfh-web::with-html-elements
		 (span (id . "(progn tag-id)")
                       (style . "margin: 5px;margin-top: 5px;display:inline-block;")
                       (a "(ps:@ tag text)"))))
           t)
       selected-tags)))
  t)

(define-for-ps return-from-tag-ui (id-prefix)
  (show-tag-area id-prefix f)
  (cond
    ;; how to "focus" on a div?
    ((= "new-todo-" id-prefix)
     (selected-tag-ids-for-current-todo 'initialize-tag-ids (get-currently-selected-tag-ids "new-todo-"))
     (render-tag-content-for-new-todo)
     (ps:chain (ps:chain document (get-element-by-id "todo-content")) (focus)))
    ((= "edit-todo-" id-prefix)
     (selected-tag-ids-for-current-todo 'initialize-tag-ids (get-currently-selected-tag-ids "edit-todo-"))
     (render-tag-content-for-edit-todo)
     (ps:chain (ps:chain document (get-element-by-id "todo-content")) (focus)))))

(define-for-ps render-selected-tags (selected-tag-ids &optional (candidate-tag-id-prefix ""))
  "render the tags selected to go with the current todo item"
  (let* ((parent-element (ps:chain document (get-element-by-id (+ candidate-tag-id-prefix "selected-tags"))))
	 (all-tags (get-all-tags))
	 (selected-tags (map* #'(lambda (selected-tag-id) (find-if* #'(lambda (tag) (= (ps:@ tag id) selected-tag-id)) all-tags)) selected-tag-ids))
         (selected-tags-element parent-element)
         (import-selected-tags (ps:chain document (get-element-by-id "import-selected-tags"))))
    (when selected-tags-element
      (clear-children parent-element)
      (cond
        ((= "import-todo-" candidate-tag-id-prefix)
         (jfh-web::with-html-elements
             (div (div (style . "margin: 5px;") "Tags to associate with imported To-Do Items:"))))
        ((= "filter-" candidate-tag-id-prefix)
         (jfh-web::with-html-elements
             (div
              (span (style . "margin: 5px;margin-top: 5px;padding: 2px;display:inline-block;") "Match:")
              (span (id . "match-any")
                    (style . "color:red; margin-left:10px; margin-right:15px; text-decoration:underline;")
                    (onclick . "(set-filter-tag-match-type-and-re-render-filter 'any)") "ANY")
              (span "|")
              (span (id . "match-all")
                    (style . "margin-left:15px; margin-right:15px; text-decoration:underline;")
                    (onclick . "(set-filter-tag-match-type-and-re-render-filter 'all)") "ALL"))))
        (t
         (jfh-web::with-html-elements
             (div (style . "margin: 5px;") (progn "Tags associated with this To-Do Item:"))))))
    (when import-selected-tags
      (setf (ps:@ import-selected-tags value) selected-tag-ids))
    (map*
     #'(lambda (tag)
         (let ((tag-id (+ candidate-tag-id-prefix *selected-tag-text* (ps:@ tag id))))
           (jfh-web::with-html-elements
               (span (id . "(progn tag-id)")
                     (style . "margin: 5px;margin-top: 5px;border-style:double;border-color:green;padding: 10px;display:inline-block;")
                     (a (onclick . "(remove-tag-from-selected tag candidate-tag-id-prefix)") "(ps:@ tag text)"))))
         t)
     selected-tags)
    (when selected-tags
      (jfh-web::with-html-elements
	  (div
	   (p (style . "margin: 5px;margin-top: 5px;padding: 10px;display:inline-block;") "Click a tag to remove it.")
	   (p (style . "float:right;margin: 5px;margin-top: 5px;padding: 10px;display:inline-block;") (a (onclick . "(return-from-tag-ui candidate-tag-id-prefix)") "Return"))))))
  t)

(define-for-ps remove-tag-from-candidate-list (tag-id id-prefix)
  "Remove tag from candidate list in UI."
  (let ((tag-list-element-id (+ id-prefix *candidate-tag-text* tag-id)))
    (ps:chain (ps:chain document (get-element-by-id tag-list-element-id)) (remove)))
  t)

(define-for-ps get-currently-selected-tag-ids (id-prefix)
  "Get selected tags from UI elements."
  (labels ((get-tag-id-from-element-id (element-id)
             (let ((regex (new (-reg-exp "\\d+$")))) ;; note: needed the "$" because for todo edits, there can be a number in the middle of the element ID
               (when (ps:chain regex (test element-id))
                 (parse-int (aref (ps:chain element-id (match regex)) 0)))))
           (get-selected-tag-ids-from-ui-elements ()
             (let* ((selected-tag-elements (ps:chain document (query-selector-all (+ "span[id^=" id-prefix *selected-tag-text*))))
                    (iterable-selected-tag-elements (ps:chain -array (from selected-tag-elements)))
                    (selected-tag-element-ids (map* #'(lambda (element) (ps:@ element id)) iterable-selected-tag-elements))
                    (selected-tag-ids (map* #'get-tag-id-from-element-id selected-tag-element-ids)))
               selected-tag-ids)))
    (if (= "filter-" id-prefix)
        (get-selected-filter-tag-ids)
        (get-selected-tag-ids-from-ui-elements))))

(define-for-ps move-tag-from-candidate-to-selected (tag id-prefix)
  "Add tag to the list of selected tags."
  (let ((tag-id (ps:@ tag id)))
    (remove-tag-from-candidate-list tag-id id-prefix)
    (let ((selected-tag-ids (get-currently-selected-tag-ids id-prefix)))
      (when (/= "filter-" id-prefix)
        (ps:chain selected-tag-ids (push tag-id))
        (adjoin-selected-tag-id-to-selected-filter-tag-ids tag-id))
      (render-selected-tags selected-tag-ids id-prefix)))
  t)

;; (define-for-ps add-tag-to-todo (tag id-prefix)
;;   "Add tag to a todo item's list of tags. Server not updated."
;;   (let* ((tag-id (ps:@ tag id))
;;          (tag-list-element-id (+ id-prefix *candidate-tag-text* tag-id))
;;          (selected-tags-element (ps:chain document (get-element-by-id (+ *selected-tag-text* tag-id))))
;;          (selected-tag-ids (ps:@ selected-tags-element value))) ;; this is wrong; need to stash a hidden field with the tag IDs for each todo so that this works
;;     (ps:chain selected-tag-ids (push tag-id))
;;     (ps:chain (ps:chain document (get-element-by-id tag-list-element-id)) (remove))
;;     ;; (add-associate-tag-to-todo (ps:create tag-id tag-id todo-id todo-id))
;;     (render-selected-tags selected-tag-ids id-prefix))
;;   t)

(define-for-ps remove-tag-from-selected (tag id-prefix)
  "Delete a tag from the selected tag list"
  (let* ((tag-id (ps:@ tag id))
         (tag-list-element-id (+ id-prefix *selected-tag-text* tag-id)))
    (ps:chain (ps:chain document (get-element-by-id tag-list-element-id)) (remove))
    (display-candidate-tag tag (ps:chain document (get-element-by-id (+ id-prefix "tag-candidates"))) id-prefix #'search-for-tag)
    (when (= "filter-" id-prefix)
      (remove-tag-id-from-selected-filter-tag-ids tag-id)
      (render-filter-tag-todos id-prefix)))
  t)
  
(define-for-ps remove-tag-from-todo (tag todo-id id-prefix)
  "Delete a tag from a todo item's tag list. Might update server?"
  (let* ((tag-id (ps:@ tag id))
         (tag-list-element-id (+ *selected-tag-text* tag-id)))
    (delete-tag-todo tag-id todo-id)
    (ps:chain (ps:chain document (get-element-by-id tag-list-element-id)) (remove))
    (display-candidate-tag tag (ps:chain document (get-element-by-id (+ id-prefix "tag-candidates"))) id-prefix #'search-for-tag)
    (let ((remove-tag-index (get-tag-todo-index-by-id (get-all-tag-todos) tag-id todo-id)))
      (when (>= remove-tag-index 0)
        (remove-tag-todo-by-index (remove-tag-index))))
    
    ;; (let ((remove-tag-index (ps:chain selected-tag-ids (find-index #'(lambda (selected-tag-id) (= tag-id selected-tag-id))))))
    ;;   (when (>= remove-tag-index 0)
    ;;     (ps:chain selected-tag-ids (splice remove-tag-index 1))
    ;;     (setf *selected-tag-ids* selected-tag-ids)))
    (when (= "filter-" id-prefix)
      (remove-tag-id-from-selected-filter-tag-ids tag-id)
      (render-filter-tag-todos id-prefix)))
  t)

(define-for-ps display-candidate-tag (candidate-tag parent-element &optional (candidate-tag-id-prefix "") (tag-click-handler #'move-tag-from-candidate-to-selected))
  "displays 1 candidate tag"
  (let ((candidate-tag-id (+ candidate-tag-id-prefix *candidate-tag-text* (ps:@ candidate-tag id))))
    (jfh-web::with-html-elements
        (span (id . "(progn candidate-tag-id)")
         (style . "margin: 5px;margin-top: 5px;border-style:double;border-color:green;padding: 10px;display:inline-block;")
         (a (onclick . "(tag-click-handler candidate-tag candidate-tag-id-prefix)") "(ps:@ candidate-tag text)"))
      ))
  t)

(define-for-ps render-tag-candidates (candidate-tags parent-element &optional (candidate-tag-id-prefix "") (tag-click-handler #'move-tag-from-candidate-to-selected) (is-search f))
  "Renders list of tags that are candidates to be added to a todo, or used in the page level filter."
  (clear-children parent-element)
  (let* ((is-filter (= "filter-" candidate-tag-id-prefix))
         (tag-candidate-prompt (if is-filter "Choose tags to filter on:" "Choose tags to associate with this To-Do Item:")))
    (jfh-web::with-html-elements
        (div
         (span (style . "margin: 5px;margin-top: 5px;padding: 2px;display:inline-block;") tag-candidate-prompt))))
  (labels ((get-candidate-tags ()
             (let* ((show-more (tag-mru-items 'get-show-more))
                    (candidate-tags-all-or-top (if is-search candidate-tags (if show-more (get-all-tags) (tag-mru-items 'get-tags-in-mru candidate-tags)))))
               (if (and is-filter (get-selected-filter-tag-ids))
                   (remove-if-not*
                    (lambda (tag) (< (position-if* (lambda (app-tag-id) (= (ps:@ tag id) app-tag-id)) (get-selected-filter-tag-ids))
                                     0))
                    candidate-tags-all-or-top)
                   candidate-tags-all-or-top))))
    (map* #'(lambda (candidate-tag) (display-candidate-tag candidate-tag parent-element candidate-tag-id-prefix tag-click-handler) t) (get-candidate-tags))
    (flet ((toggle-show-more ()
             (tag-mru-items 'update-show-more (not (tag-mru-items 'get-show-more)))
             (render-tag-candidates (get-candidate-tags) parent-element candidate-tag-id-prefix tag-click-handler))) ;; can we use ARGUMENTS somehow?
      (let ((show-text (if (tag-mru-items 'get-show-more) "Show Less" "Show More")))
        (jfh-web::with-html-elements
            (div (style . "margin: 5px;margin-top: 5px; padding: 10px;") (a (onclick . "(toggle-show-more)")  show-text))))))
  t)

(var *show-tag-content-handler*)

(define-for-ps render-tag-content-for-new-todo (event)
  "Render tag content to use when adding a new todo item."
  (populate-selected-tags-content-area-for-todos "new-todo-")
  (render-tag-summary "new-todo-"))

(define-for-ps render-tag-content-for-edit-todo (todo-id index)
  "Render tag content to use when editing a todo item."
  ;; (render-tag-content (+ "edit-todo-" index "-") todo-id)
  (let ((id-prefix "edit-todo-";; (+ "edit-todo-" index "-")
		   ))
    (render-tag-filter-ui-by-area-independent id-prefix)
    (render-tag-area id-prefix todo-id)
    (populate-selected-tags-content-area-for-todos id-prefix todo-id)
    (render-tag-summary id-prefix)))

(define-for-ps populate-selected-tags-content-area-for-todos (id-prefix &optional todo-id)
  (labels ((get-new-todo-selected-tag-ids-from-global-filter ()
	     (let ((global-selected-tags (remove-if-not* (lambda (tag) (find* (ps:@ tag id) (get-selected-filter-tag-ids))) (get-all-tags))))
	       (map* (lambda (tag) (ps:@ tag id)) global-selected-tags))))
    (flet ((get-selected-tag-ids-for-this-todo ()
	     (let ((selected-tag-ids-for-this-todo (if todo-id (get-tag-id-list-by-todo-id (get-all-tag-todos) todo-id) (get-all-selected-tag-ids-for-current-todo))))
	       (if (and selected-tag-ids-for-this-todo (> (length selected-tag-ids-for-this-todo) 0))
		   selected-tag-ids-for-this-todo
		   ;; (if todo-id
		   ;;     (get-tag-id-list-by-todo-id (get-all-tag-todos) todo-id)
		   ;;     (get-new-todo-selected-tag-ids-from-global-filter))
		   (get-new-todo-selected-tag-ids-from-global-filter))))
	   (get-selected-tag-ids-by-todo-type (id-prefix &optional todo-id)
	     (cond ((= "new-todo-")
		    (initialize-tag-ids-for-todo-item "new-todo"))
		   ((= "edit-todo-")
		    (initialize-tag-ids-for-todo-item "edit-todo")))
	     (get-all-selected-tag-ids-for-current-todo)))
      (let ((tag-ids-for-this-todo (get-selected-tag-ids-by-todo-type id-prefix)
	      ;; (get-selected-tag-ids-for-this-todo)
				   ))
	(render-selected-tags tag-ids-for-this-todo id-prefix)
	(selected-tag-ids-for-current-todo 'initialize-tag-ids tag-ids-for-this-todo)))))

(define-for-ps render-tag-content (id-prefix &optional todo-id)
  "render tag entry, selected tags, and tag candidates."
  (let ((tag-content-area (if todo-id (ps:chain document (get-element-by-id "edit-todo-tag-content")) (get-tag-content-area-element id-prefix)))
        (tag-candidate-area (ps:chain document (get-element-by-id (+ id-prefix "tag-candidates")))))
    (labels ((search-tags (event)
               (let* ((search-input (ps:@ (ps:chain document (get-element-by-id (+ id-prefix "tag-input"))) value))
                      (search-results (get-tags-matching-search-input (get-all-tags) search-input)))
                 (render-tag-candidates search-results tag-candidate-area id-prefix #'move-tag-from-candidate-to-selected t))
               t))
      (flet ((tag-content-visible ()
               (not (ps:@ tag-content-area hidden)))
             (render-tag-entry ()
               (let ((parent-element tag-content-area))
                 (jfh-web::with-html-elements
                     (div
                      (div (class . "tag-display") (style . "border-color: green; border-style: solid;")
                           (input (type . "text") (id . "(+ id-prefix \"tag-input\")") (placeholder . "add new tag") (oninput . "(search-tags)") (style . "margin-left: 5px;"))
                           (button (style . "margin-left: 30px;") (onclick . "(add-tag id-prefix)") "Add Tag"))
                      (div (id . "(+ id-prefix \"selected-tags\")") (class . "tag-display") (style . "border-color: green; border-style: solid;")))))
               t)
             (show-tag-content-area (show)
               (let ((hide (not show)))
                 (setf (ps:@ tag-content-area hidden) hide)))
             (tag-elements-already-exist ()
               (ps:chain document (get-element-by-id (+ id-prefix "tag-input"))))
	     (get-new-todo-selected-tag-ids-from-global-filter ()
	       (let ((global-selected-tags (remove-if-not* (lambda (tag) (find* (ps:@ tag id) (get-selected-filter-tag-ids))) (get-all-tags))))
		 (map* (lambda (tag) (ps:@ tag id)) global-selected-tags))))
        (unless (tag-content-visible)
          (show-tag-content-area t)
          (setf *show-tag-content-handler* #'(lambda () (show-tag-content-area ps:f)))
          (unless (tag-elements-already-exist)
            (render-tag-candidates (get-all-tags) tag-candidate-area id-prefix)
            (render-tag-entry)
            (let ((tag-ids-for-this-todo (if todo-id (get-tag-id-list-by-todo-id (get-all-tag-todos) todo-id) (get-new-todo-selected-tag-ids-from-global-filter))))
              (render-selected-tags tag-ids-for-this-todo id-prefix)
              (selected-tag-ids-for-current-todo 'initialize-tag-ids tag-ids-for-this-todo)))))))
  t)

(define-for-ps render-todo-list (todos)
  "Render html elements for todo list."
  (let* ((todo-list-table-body (ps:chain document (get-element-by-id "todo-list-body")))
         (parent-element todo-list-table-body)
         (column-header (ps:chain document (get-element-by-id "todo-list-column-header")))
         (filter-text (@ (ps:chain document (get-element-by-id "todo-filter-text")) value))
         (filtered-todos  (remove-if-not*
                           #'(lambda (todo)
                               (and
                                (or (not (@ (app-settings 'get-app-settings) hide-done-items))
                                    (not (@ todo done)))
                                (ps:chain (@ todo text) (match (new (-reg-exp filter-text "i"))))))
                           todos))
         (count (length filtered-todos))
         (use-plural-form (or (> count 1) (= 0 count)))
         (checked-count (length (remove-if-not* #'(lambda (todo) (@ todo done)) filtered-todos))))
    (clear-children parent-element)
    (setf (ps:chain column-header inner-text)
          (+ (if use-plural-form "To-do Items" "To-do Item") " " (ps:chain checked-count (to-string)) "/" (ps:chain count (to-string))))
    (map*
     #'(lambda (todo index)
         (let ((todo-checkbox-id (+ *todo-checkbox* index))
               (todo-label-id (+ *todo-label* index))
               (show-todo-edit-class-name (+ *show-todo-edit* index))
               (hide-todo-edit-class-name (+ *hide-todo-edit* index))
               (todo-text-id (+ *todo-text* index))
               (tag-content-id (+ "edit-todo-" index "-tag-content"))
               (tag-candidates-id (+ "edit-todo-" index "-tag-candidates"))
               (pre-style (if (@ todo done) "text-decoration: line-through;color: #888;display:inline;" "display:inline;")))
           (labels ((show-input-for (todo show-edit)
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
                        (update-todo-from-edit todo index)
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
                          (div (id . "(progn tag-candidates-id)") (class . "tag-display") (style . "border-color:green;border-style:solid;")))
                     (span (br (ref . "(progn index)")))
                     (button (hidden . "t") (onclick . "(save-input-for todo)") (class . "(progn show-todo-edit-class-name)") "Save")
                     (span "  ")
                     (button (hidden . "t") (onclick . "(delete-todo todo)") (class . "(progn show-todo-edit-class-name)") "Delete"))))))
           t))
     filtered-todos)))

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
   ;; (defparameter *tags-todo-association-list* ([]))
   (defparameter *tags* (make-tags ([])))
   (defparameter *tag-todos* (make-tag-todos ([])))
   (defparameter *selected-tag-ids-for-current-todo* (make-selected-tag-ids-for-current-todo (list)))
   (defparameter *tag-mru* (make-tag-mru ([])))))

(defun client-ui-import ()
  "define client side UI functions"
  (ps:ps
    (defparameter *candidate-tag-text* "candidate-tag")
    (defparameter *selected-tag-text* "selected-tag")

    (defparameter *selected-tag-ids* (list))
    ;; (defparameter *selected-filter-tag-todo-ids* (list))
    ;; (defparameter *max-candidate-tag-show-count* 1000)
    ;; (defparameter *filter-tag-match-type* 'any)

    (setf (ps:chain window onload) init-import)))

(define-for-ps init-import ()
  "initialize html elements and JS objects on page load"
  (with-callback
    ;;   (get-app-settings-from-server)
    ;; (get-todo-list-from-server)
    (get-tag-list-from-server)
    ;; (get-tag-todo-associaton-list-from-server)
    (get-tag-mru-list-from-server)
    )
  (let ((todo-content (ps:chain document (get-element-by-id "import-list"))))
    ;; (ps:chain add-button (add-event-listener "click" add-todo false))
    (ps:chain todo-content (add-event-listener "input" render-tag-content-for-import-todo todo-content event false))))


(define-for-ps render-tag-content-for-import-todo (input event)
  (render-tag-content "import-todo-"))
