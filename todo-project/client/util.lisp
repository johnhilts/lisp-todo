(in-package #:todo-project)

(defun client-util ()
  "define client side utility functions"
  (ps

    (defun get-next-index (todo-list)
      "calculate next index for todo list"
      (let ((id-list (chain todo-list (map #'(lambda (todo) (@ todo id)))))
            (max-fn (@ -Math max)))
        (if (length id-list)
            (+ 1 (chain max-fn (apply null id-list)))
            1)))))
