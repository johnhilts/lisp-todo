(defun app-entry ()
  (with-chained-callback
      (fetch-app-settings)
    (fetch-todos)
    (fetch-tags)
    (fetch-tag-todos)

    (render data
            (filter-area)
            (todo-area))))

(todo-lambda
          (get-all-todos (data todos))
         (add-todo (data input))
         (update-todo (data input))
         (delete-todo todo-id)
         (filter-todos (data input)))

(tag-lambda
         (get-all-tags (data tags))
         (get-all-tag-candidates)
         (get-all-selected-tags)
         
         (add-tag (data input))
         (update-tag (data input))
         (delete-tag tag-id)

         (select-tag-from-candidates tag-id)
         (remove-tag-from-selected-tags tag-id))

(tag-todo lambda
          (get-all-tag-candidates todo-id)
          (get-all-selected-tags todo-id)
          (select-tag-from-candidates tag-id todo-id)
          (remove-tag-from-selected-tags tag-id todo-id))
