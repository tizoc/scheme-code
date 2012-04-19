(module (tizoc tnetstrings)
  (export read-tnetstring parse-tnetstring write-tnetstring unparse-tnetstring)
  (import-immutable (scheme) (srfi 39) (srfi 69) (chibi io))
  (include "tnetstrings/tnetstrings.scm"))
