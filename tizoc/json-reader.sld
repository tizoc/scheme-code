(module (tizoc json-reader)
  (export
   read-json-object
   json-string->alist)
  (import-immutable (scheme))
  (import (srfi 9) (srfi 39))
  (include "json-reader/json-reader.scm"))