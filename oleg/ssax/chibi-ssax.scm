;; Extra definitions required by SSAX.scm

(define ucscode->char integer->char)
(define ascii->char integer->char)
(define char-return (ascii->char 13))
(define char-tab (ascii->char 9))
(define char-newline (ascii->char 10))

(define parser-error error)
