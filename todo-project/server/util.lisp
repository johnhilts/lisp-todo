(in-package #:todo-project)

(defun split-list (list first-part-length)
  "split a list into 2 parts"
  (list
   (subseq list 0 first-part-length)
   (subseq list first-part-length)))

(defun split-string-by (delimiter string)
  (flet ((trim (string &optional return-type)
           (let ((trimmed (string-trim '(#\Space) string)))
             (if (zerop (length trimmed))
                 (if (equal 'list return-type) () "")
                 (if (equal 'list return-type) (list trimmed) trimmed)))))
    (let ((delimiter-position (position delimiter string)))
      (if delimiter-position
          (let ((trimmed (trim (subseq string 0 delimiter-position)))
                (split-next #'(lambda ()
                                (split-string-by
                                 delimiter
                                 (subseq string (+ 1 delimiter-position))))))
            (if (zerop (length trimmed))
                (funcall split-next)
                (cons trimmed (funcall split-next))))
          (trim string 'list)))))

(defun splice-and-replace-item-in-list (list item-to-replace replace-item-position)
  "splice list and replace item at position where splitting"
  (let ((split-list (split-list list replace-item-position)))
    (append (car split-list) (list item-to-replace) (cdadr split-list))))

(defun splice-and-remove-item-in-list (list remove-item-position)
  "splice list and remove item at position where splitting"
  (let ((split-list (split-list list remove-item-position)))
    (append (car split-list) (cdadr split-list))))

(defun string-replace (string search replace)
  "replace part of a string
usage:
(string-replace \"main string\" \"search\" \"replace\")
example:
(string-replace \"abcd1234\" \"cd12\" \"xyz567\")
=> \"abxyz56734\"
key points:
- The search and replace parameters don't need to be the same length
- only the part of the string that matches search will be replaced."
  (labels
      ((replace-r (string search replace)
         (if (zerop (length string))
             string
             (let ((search-position (search search string)))
               (if (null search-position)
                   string
                   (concatenate 'string
                                (subseq string 0 search-position)
                                replace
                                (replace-r (subseq string  (+ search-position (length search))) search replace)))))))
    (replace-r string search replace)))

(defun encode-plist-to-json-as-string (plist)
  "wrapper to cl-json's encode-json-plist-to-string to support JS false"
  (let ((json (json:encode-json-plist-to-string plist)))
    (string-replace
     (string-replace
      json
      "\"done\":0," "\"done\":false,")
     "\"hideDoneItems\":0" "\"hideDoneItems\":false")))

(defun encode-multiple-plists-to-json-as-string (plists)
  "wrapper to this app's encode-plist-to-json-as-string to support a list of plists"
  (labels
      ((concat-plists (plists)
         (cond
           ((= 1 (length plists))
            (encode-plist-to-json-as-string (car plists)))
           (t (concatenate 'string
                           (encode-plist-to-json-as-string (car plists))
                           ", "
                           (concat-plists (cdr plists)))))))

    (if plists
        (concatenate 'string
                     "["
                     (concat-plists plists)
                     "]")
        "[]")))

(defun convert-nil-to-zero (keyword value)
  "convert nill to zero to support interop between app and cl-json"
  (list keyword (if (and (equal :done keyword) (null value)) 0 value)))

(defun join-pairs (acc cur)
  "put plist pairs in same list - can the same thing be done with mapcan??"
  (append
   acc
   (convert-nil-to-zero
    (car cur)
    (cdr cur))))

(defun fetch-or-create-data (file-path &optional call-back)
  "read data from persistence store; call call back if provided"
  (let ((data (read-complete-file file-path)))
    (if call-back
        (funcall call-back data)
        data)))

(defun convert-dotted-pair-to-plist (input)
  "convert list of cons dotted pairs (input) to plist (app-specific format)"
  (reduce #'join-pairs  input :initial-value ()))

(defun generate-unique-token ()
  "create a token based on the date and RANDOM"
  (flet ((coalesce (original coalesced)
           (if (zerop original) coalesced original)))
    (let* ((date (get-parsed-date (make-instance (define-info-class date second minute hour day month year day-of-the-week daylight-p zone))))
           (hour (coalesce (date-hour date) (1+ (random 24))))
           (minute (coalesce (date-minute date) (1+ (random 60))))
           (second (coalesce (date-second date) (1+ (random 60))))
           (salt (random (* hour minute second)))
           (date-based-random-number (format nil "~s~s~d~s~s~s~s"
                                             (* salt (date-month date))
                                             (* salt (date-day date))
                                             (* salt (date-day-of-the-week date))
                                             (* salt (date-zone date))
                                             (date-hour date)
                                             (date-minute date)
                                             (date-second date))))
      (format nil "~a-~a-~a-~a"
              (subseq date-based-random-number 0 7)
              (format nil "~5,'0d" (random (- (expt 10 5) 1)))
              (subseq (reverse date-based-random-number) 0 7)
              (format nil "~5,'0d" (random (- (expt 10 5) 1)))))))

(defun format-string (&rest strings)
  "More concise version of concatenate 'string"
  (apply #'concatenate (cons 'string strings)))

(defun hash-password (password)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array password))))
