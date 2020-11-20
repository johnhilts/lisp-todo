
(in-package #:todo-project)

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
  (string-replace (json:encode-json-plist-to-string plist) "\"done\":0," "\"done\":false,"))

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

(defun iterate-through-pairs (acc cur)
  "iterate through plist pairs"
  (append
        acc
        (list (reduce #'join-pairs cur :initial-value ()))))
