(define-library (tizoc json-reader)
  (export read-json-object json-string->alist)
  (import (scheme) (srfi 9) (srfi 33) (srfi 39))
  (include "json-reader/json-reader.scm"))
