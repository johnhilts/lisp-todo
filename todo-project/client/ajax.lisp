(in-package #:todo-project)

(defun ajax ()
  "define all AJAX related functions"
  (ps

    (defun send-to-server (the-url http-method data)
      "Do AJAX request using POST or PUT"
      (let ((xml-http (new (-x-m-l-http-request))))
        (chain xml-http (open http-method the-url))
        (chain xml-http (set-request-header "Content-Type" "application/json;charset=UTF-8"))
        (chain xml-http (send (chain -j-s-o-n (stringify data)))))
      t)

    
    (defun get-from-server (the-url call-back)
      "Do AJAX request using GET"
      (let ((http-request (new (-x-m-l-http-request))))
        (chain http-request (add-event-listener "load" call-back))
        (chain http-request (open "GET" the-url))
        (chain http-request (send)))
      t)))
