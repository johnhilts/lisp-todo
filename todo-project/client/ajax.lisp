(in-package #:todo-project)

(define-for-ps send-to-server (the-url http-method data &optional call-back)
  "Do AJAX request using POST or PUT"
  (let ((xml-http (new (-x-m-l-http-request))))
    (when call-back
      (chain xml-http (add-event-listener "load" call-back)))
    (chain xml-http (open http-method the-url))
    (chain xml-http (set-request-header "Content-Type" "application/json;charset=UTF-8"))
    (chain xml-http (send (chain -j-s-o-n (stringify data)))))
  t)

    
(define-for-ps get-from-server (the-url call-back)
  "Do AJAX request using GET"
  (let ((http-request (new (-x-m-l-http-request))))
    (chain http-request (add-event-listener "load" call-back))
    (chain http-request (open "GET" the-url))
    (chain http-request (send)))
  t)
