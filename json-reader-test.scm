(import (tizoc chibi json-reader) (chibi test) (chibi io))

(define json-null (json-string->alist "null"))

(define expected-alist-result
  `(("string" . "string value\nwith newline")
    ("float" . 12345.0)
    ("integer" . 12345)
    ("null" . ,json-null)
    ("true" . #t)
    ("false" . #f)
    ("array" . ("string" 1.0 1 ,json-null #t #f))))

(test
 "json-string->alist with map"
 expected-alist-result
 (let ((data (call-with-input-file "json-test.json"
               port->string)))
   (json-string->alist data)))

(test
 "json-string->alist with number"
 12345.0
 (let ((data "12345.0"))
   (json-string->alist data)))

(test
 "json-string->alist with null"
 json-null
 (let ((data "null"))
   (json-string->alist data)))

(test
 "json-string->alist with array"
 '(12345.0 "string" (("key" . #t)))
 (let ((data "[12345.0, \"string\", {\"key\": true}]"))
   (json-string->alist data)))

(test
 "json-string->alist with empty string"
 #f
 (json-string->alist ""))
