;; Utils

(define (with-input-from-port port thunk)
  (parameterize ((current-input-port port))
    (thunk)))

(define (with-input-from-string string thunk)
  (call-with-input-string string
    (lambda (in)
      (with-input-from-port in thunk))))

;;

(define-record-type json-null-object
  (%make-json-null-object)
  %json-null-object?)

(define *json-null-object* (%make-json-null-object))

(define (json-null-object? object)
  (eq? object *json-null-object*))

(define (read-error char)
  (error "got unexpected character" char))

(define (parser-error object)
  (error "got unexpected object" object))

(define (read-token)
  (let ((char (read-char)))
    (cond
     ((eof-object? char) char)
     ((char-whitespace? char) (read-token))
     ((or (char-numeric? char)
          (eq? char #\-)
          (eq? char #\+)
          (eq? char #\.))
      (read-number char))
     (else
      (case char
        ((#\") (read-string))
        ((#\:) 'key-separator)
        ((#\,) 'value-separator)
        ((#\{) 'start-map)
        ((#\}) 'end-map)
        ((#\[) 'start-list)
        ((#\]) 'end-list)
        ((#\t) (read-true))
        ((#\f) (read-false))
        ((#\n) (read-null))
        (else
         (read-error char)))))))

(define (expect-char expected-char)
  (let ((char (read-char)))
    (if (char=? expected-char char)
        char
        (read-error char))))

(define (expect-string chars)
  (for-each expect-char chars))

(define read-true
  (let ((chars (string->list "rue")))
    (lambda ()
      (expect-string chars)
      #t)))

(define read-false
  (let ((chars (string->list "alse")))
    (lambda ()
      (expect-string chars)
      #f)))

(define read-null
  (let ((chars (string->list "ull")))
    (lambda ()
      (expect-string chars)
      *json-null-object*)))

(define (combine-bytes b1 b2 b3 b4)
  (bitwise-ior (arithmetic-shift b1 12)
               (arithmetic-shift b2 8)
               (arithmetic-shift b3 4)
               b4))

(define (read-hex-digit)
  (let* ((char (read-char))
         (n (char->integer char)))
    (cond
     ((<= 48 n 57) (- n 48)) ;; 0-9
     ((<= 65 n 70) (- n 55)) ;; A-F
     ((<= 97 n 102) (- n 87)) ;; a-f
     (else
      (read-error char)))))

(define (read-escaped-unicode-char)
  (let* ((b1 (read-hex-digit))
         (b2 (read-hex-digit))
         (b3 (read-hex-digit))
         (b4 (read-hex-digit)))
    (integer->char (combine-bytes b1 b2 b3 b4))))

(define (read-escaped-char)
  (let ((char (read-char)))
    (case char
      ((#\\ #\") char)
      ((#\b) (integer->char 8))
      ((#\f) (integer->char 12))
      ((#\n) #\newline)
      ((#\r) #\return)
      ((#\t) #\tab)
      ((#\u) (read-escaped-unicode-char))
      (else
       (read-error char)))))

(define (control-char? char)
  (let ((ord (char->integer char)))
    (or (< ord 32) (< 127 ord 160))))

(define (read-string)
  (let read-string-loop ((res '()))
    (let ((char (read-char)))
      (case char
        ((#\\) (read-string-loop (cons (read-escaped-char) res)))
        ((#\") (list->string (reverse res)))
        (else
         (if (control-char? char)
             (read-error char)
             (read-string-loop (cons char res))))))))

(define (numeric? ch)
  (or (char-numeric? ch)
      (eq? ch #\.)
      (eq? ch #\e)
      (eq? ch #\E)))

(define (read-number first-char)
  (let read-number-loop ((res (list first-char)))
    (let ((char (peek-char)))
      (if (and (not (eof-object? char)) (numeric? char))
          (read-number-loop (cons (read-char) res))
          (string->number (list->string (reverse res)))))))

(define (read-list)
  (let read-list-loop ((res '()))
    (let ((token (read-token)))
      (case token
        ((end-list) (reverse res))
        (else
         (let* ((res (cons (read-json-object-complete token) res))
                (next-token (read-token)))
           (case next-token
             ((end-list) (reverse res))
             ((value-separator) (read-list-loop res))
             (else
              (parser-error next-token)))))))))

(define (read-map-value)
  (let ((token (read-token)))
    (if (eq? token 'key-separator)
        (read-json-object)
        (parser-error token))))

(define (read-map)
  (let read-map-loop ((res '()))
    (let ((token (read-token)))
      (cond
       ((eq? token 'end-map) (reverse res))
       ((string? token)
        (let* ((key+value (cons token (read-map-value)))
               (res (cons key+value res))
               (next-token (read-token)))
          (case next-token
            ((end-map) (reverse res))
            ((value-separator) (read-map-loop res))
            (else
             (parser-error next-token)))))
       (else
        (parser-error token))))))

(define (read-json-object-complete token)
  (cond
   ((eq? token 'start-map) (read-map))
   ((eq? token 'start-list) (read-list))
   ((or (string? token)
        (number? token)
        (boolean? token)
        (json-null-object? token))
    token)
   ((eof-object? token) #f)
   (else
    (parser-error token))))

(define (read-json-object)
  (read-json-object-complete (read-token)))

(define (json-string->alist string)
  (with-input-from-string string read-json-object))
