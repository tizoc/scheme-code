;; Some utilities

(define (read-until-char delimiter port)
  (let loop ((acc '()))
    (let ((ch (read-char port)))
      (cond
       ((or (eof-object? ch) (char=? ch delimiter))
        (list->string (reverse acc)))
       (else
        (loop (cons ch acc)))))))

(define drop-char read-char)

(define (string-empty? str)
  (zero? (string-length str)))

;; Parser code

(define (read-tnetstring port)
  (if (eof-object? (peek-char port))
      (read-char port)
      (let* ((slen (read-until-char #\: port))
             (len (string->number slen))
             (data (read-string len port))
             (type (read-char port)))
        (case type
          ((#\,) data)
          ((#\#) (string->number data))
          ((#\~) #f)
          ((#\!) (cond ((string=? data "true") #t)
                       ((string=? data "false") #f)
                       (else (error "invalid boolean"))))
          ((#\]) (parse-tnetstring-list data))
          ((#\}) (parse-tnetstring-dict data))
          (else (error "invalid payload type" type))))))

(define (read-tnetstring-pair port)
  (let ((key (read-tnetstring port)))
    (if (eof-object? key)
        key ;; is EOF
        (let ((value (read-tnetstring port)))
          (if (eof-object? value)
              (error "Unbalanced tnetstring pair")
              (cons key value))))))

(define (read-tnetstring-list port)
  (let loop ((result '()) (value (read-tnetstring port)))
    (if (eof-object? value)
        (reverse result)
        (loop (cons value result)
              (read-tnetstring port)))))

(define (read-tnetstring-dict port)
  (let ((result (make-hash-table string=?)))
    (let loop ((key+value (read-tnetstring-pair port)))
      (cond ((eof-object? key+value) result)          
            (else
             (hash-table-set! result (car key+value) (cdr key+value))
             (loop (read-tnetstring-pair port)))))))

(define (read-from-string/empty string empty-default proc)
  (if (string-empty? string)
      (empty-default)
      (call-with-input-string string proc)))

(define (parse-tnetstring-list string)
  (read-from-string/empty string (lambda () '()) read-tnetstring-list))

(define (empty-hash-table) (make-hash-table string=?))

(define (parse-tnetstring-dict string)
  (read-from-string/empty string empty-hash-table read-tnetstring-dict))

(define (parse-tnetstring string)
  (if (string-empty? string)
      (error "Invalid tnetstring, it is empty")
      (call-with-input-string string read-tnetstring)))

;; Unparser code

(define (write-tnetstring value port)
  (cond ((integer? value) (write-tnetstring-value (number->string value) #\# port))
        ((string? value) (write-tnetstring-value value #\, port))
        ((list? value) (write-tnetstring-list value port))
        ((hash-table? value) (write-tnetstring-dict value port))
        ((eq? value #t) (write-tnetstring-value "true" #\! port))
        ((eq? value #f) (write-tnetstring-value "false" #\! port))
        (else (error "Failed to serialize value to tnetstring: " value))))

(define (write-tnetstring-value data type-char port)
  (for-each (lambda (v) (display v port))
            (list (string-length data) #\: data type-char)))

(define (write-tnetstring-list lst port)
  (let ((data (call-with-output-string
               (lambda (port)
                 (for-each (lambda (v) (write-tnetstring v port)) lst)))))
    (write-tnetstring-value data #\] port)))

(define (write-tnetstring-dict hashtable port)
  (let ((data (call-with-output-string
               (lambda (port)
                 (hash-table-walk hashtable
                   (lambda (key value)
                     (write-tnetstring key port)
                     (write-tnetstring value port)))))))
    (write-tnetstring-value data #\} port)))

(define (unparse-tnetstring value)
  (call-with-output-string (lambda (port) (write-tnetstring value port))))
