(in-package #:todo-project)

(defparameter *registered-ps-functions* ())

(defmacro define-for-ps (name args &body body)
  (list 'progn
        `(defun ,name ()
           (ps
             (defun ,name (,@args)
               ,@body)))
        `(let ((function (member ',name *registered-ps-functions*)))
           (when function
             (remove (car (member ',name *registered-ps-functions*)) (remove (cadr (member ',name *registered-ps-functions*)) *registered-ps-functions*)))
           (setf (getf *registered-ps-functions* ',name) #',name))))

(defun client-side-macros ()
  (ps:ps
   (defmacro find* (item seq)
     "Support CL style find that transpiles into the correct JS forms"
     `(ps:chain ,seq (find #'(lambda (e) (= e ,item)))))

   (defmacro find-if* (predicate seq)
     "Support CL style find-if that transpiles into the correct JS forms such as find"
     `(ps:chain ,seq (find ,predicate)))

   (defmacro remove* (item seq)
     "Support CL style remove that transpiles into the correct JS forms"
     `(remove-if-not* #'(lambda (e) (not (= e ,item))) ,seq))

   (defmacro position-if* (predicate seq)
     "Support CL style position-if that transpiles into the correct JS forms such as findIndex"
     `(ps:chain ,seq (find-index ,predicate)))

   (defmacro map* (predicate seq)
     "Support CL style map that transpiles into the correct JS forms"
     `(ps:chain ,seq (map ,predicate)))

   (defmacro remove-if-not* (predicate seq)
     "Support CL style filtering that transpiles into the correct JS forms"
     `(ps:chain ,seq (filter ,predicate)))

   (defmacro every* (predicate seq)
     "Support CL style every that transpiles into the correct JS forms"
     `(ps:chain ,seq (every ,predicate)))

   (defmacro some* (predicate seq)
     "Support CL style every that transpiles into the correct JS forms"
     `(ps:chain ,seq (some ,predicate)))

   (defmacro lower* (string)
     "Shortand for JS version of STRING-DOWNCASE"
     `(ps:chain ,string (to-lower-case)))

   (defmacro push* (item seq)
     "Shortand for JS version of PUSH"
     `(ps:chain ,seq (push ,item)))))

(defmacro define-dispatchable-functions (name args &body body)
  (let ((function-name (read-from-string (concatenate 'string "make-" (string name)))))
    (flet ((get-dispatchable-functions (acc cur)
             (append (car cur) acc))
           (get-dispatcher (dispatchable-functions)
             `(dispatch (m)
                        (cond
                          ,@(reduce
                             #'(lambda (acc cur)
                                 (append acc `(((eq m ',(car cur)) #',(car cur)))))
                             dispatchable-functions :initial-value ())
                          (t
                           (ps:chain console (log (+ ,(concatenate 'string "Unknown request -- " (string-upcase function-name) ": ") m))))))))
      (let* ((dispatchable-functions (reduce #'get-dispatchable-functions body))
             (labels body))
        (setf (cdr (last (car labels))) (list (get-dispatcher dispatchable-functions)))
        `(define-for-ps ,function-name (,@args)
           (labels ,@labels
             #'dispatch))))))
