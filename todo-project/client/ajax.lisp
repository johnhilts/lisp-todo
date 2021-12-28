(in-package #:todo-project)

(ps
  (defmacro with-callback (fn &body body)
    `(,(car fn) ,@(cdr fn) #'(lambda (),@body))))

(define-for-ps send-to-server (the-url http-method data &optional call-back)
  "Do AJAX request using POST or PUT"
  (let ((xml-http (new (-x-m-l-http-request))))
    (when call-back
      (ps:chain xml-http (add-event-listener "load" call-back)))
    (ps:chain xml-http (open http-method the-url))
    (ps:chain xml-http (set-request-header "Content-Type" "application/json;charset=UTF-8"))
    (ps:chain xml-http (send (ps:chain -j-s-o-n (stringify data)))))
  t)

    
(define-for-ps get-from-server (the-url call-back)
  "Do AJAX request using GET"
  (let ((http-request (new (-x-m-l-http-request))))
    (ps:chain http-request (add-event-listener "load" call-back))
    (ps:chain http-request (open "GET" the-url))
    (ps:chain http-request (send)))
  t)
