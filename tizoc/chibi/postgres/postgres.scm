(define NULL 'NULL)

(define identity (lambda (value) value))

(define (parse-bool value)
  (string=? value "t"))

(define parse-integer string->number)

(define parse-float string->number)

(define (parse-char value)
  (string-ref value 0))

;; "2012-05-13 21:29:02.560071-03"
;; "%Y-%m-%d %H:%M:%S.%N%z"
;; TODO
(define (parse-date value)
  value)

(define (cursor+ str cur n)
  (if (zero? n)
      cur
      (cursor+ str (string-cursor-next str cur) (- n 1))))

(define (read-hex-u8-at str cur)
  (string->number (substring-cursor str cur (cursor+ str cur 2))
                  16))

(define (parse-bytea/hex value)
  (let ((bytes (make-bytevector (/ (- (string-length value) 2) 2)))
        (start (cursor+ value (string-cursor-start value) 2)))
    (loop continue ((for ch cur (in-string value start))
                    (for i (up-from 0)))
      => bytes
      (bytevector-u8-set! bytes i (read-hex-u8-at value cur))
      (continue (=> cur (cursor+ value cur 2))))))

(define (parse-bytea/escaped value)
  value)

(define (parse-bytea value)
  (if (string-prefix? "\\x" value)
      (parse-bytea/hex value)
      (parse-bytea/escaped value)))

(define *default-parsers*
  `((16 . ,parse-bool)    ; "boolean, 'true'/'false'"
    (17 . ,parse-bytea)   ; "variable-length string, binary values escaped"
    (18 . ,parse-char)    ; "single character"
    (20 . ,parse-integer) ; "~18 digit integer, 8-byte storage"
    (21 . ,parse-integer) ; "-32 thousand to 32 thousand, 2-byte storage"
    (23 . ,parse-integer) ; "-2 billion to 2 billion integer, 4-byte storage"
    (26 . ,parse-integer) ; "object identifier(oid), maximum 4 billion"
    (700 . ,parse-float)  ; "single-precision floating point number, 4-byte storage"
    (701 . ,parse-float)  ; "double-precision floating point number, 8-byte storage"
    (1114 . ,parse-date)  ; "date and time"
    (1184 . ,parse-date)  ; "date and time with time zone"
    ))

(define (vector-tabulate f length)
  (let ((v (make-vector length)))
    (do ((i 0 (+ i 1)))
        ((= i length) v)
      (vector-set! v i (f i)))))

(define (row-fields-parsers results)
  (vector-tabulate
   (lambda (n)
     (let ((parser (assq (PQftype results n) *default-parsers*)))
       (or (and parser (cdr parser)) identity)))
   (PQnfields results)))

(define (PQgetvalue results row col)
  (case (PQfformat results col)
    ((0) (PQgetvalue/text results row col))
    ((1) (PQgetvalue/binary results row col))))

(define (get-value results parsers row col)
  (let ((parser (vector-ref parsers col)))
    (if (PQgetisnull results row col)
        NULL
        (parser (PQgetvalue results row col)))))

(define (fold-results kons knil results)
  (let ((row-converter (make-row-converter results)))
    (loop ((with knil knil (kons (row-converter results i) knil))
           (for i (down-from (PQntuples results) (to 0))))
      => knil)))

(define (make-row-converter results)
  (let ((parsers (row-fields-parsers results)))
    (lambda (results row)
      (vector-tabulate (lambda (col) (get-value results parsers row col))
                       (PQnfields results)))))

(define (result-code-symbol code)
  (cond
   ((= code ExecStatusType/EMPTY_QUERY)    'empty-query)
   ((= code ExecStatusType/COMMAND_OK)     'command-ok)
   ((= code ExecStatusType/TUPLES_OK)      'tuples-ok)
   ((= code ExecStatusType/COPY_OUT)       'copy-out)
   ((= code ExecStatusType/COPY_IN)        'copy-in)
   ((= code ExecStatusType/BAD_RESPONSE)   'bad-response)
   ((= code ExecStatusType/NONFATAL_ERROR) 'nonfatal-error)
   ((= code ExecStatusType/FATAL_ERROR)    'fatal-error)
   ((= code ExecStatusType/COPY_BOTH)      'copy-both)))

(define (check-result result)
  (case (result-code-symbol (PQresultStatus result))
    ((empty-query)          (error "Empty query"))
    ((command-ok tuples-ok) result)
    ((copy-out)             (error "Copy out")) ;; TODO: handle
    ((copy-in)              (error "Copy in"))  ;; TODO: handle
    ((bad-response)         (error "Bad server response"))
    ((nonfatal-error)       result)
    ((fatal-error)          (error "Fatal error" (PQresultErrorMessage result)))))

(define (PQsendQueryParams/text-wait conn query params)
  (if (PQsendQueryParams/text conn query params)
      (loop continue ((with result '()))
        (cond
         ((not (PQconsumeInput conn))
          (error "PQconsumeInput failed" (PQerrorMessage conn)))
         ((PQisBusy conn)
          (thread-sleep! (/ 1.0 1000.0))
          (continue))
         (else
          (let ((more-results (PQgetResult conn)))
            (if more-results
                (let ((result (fold-results cons result
                                            (check-result more-results)) ))
                  (continue (=> result result)))
                result)))))
      (error "Query send failed" (PQerrorMessage conn))))

(define (execute-sql conn query . params)
  (PQsendQueryParams/text-wait conn query params))

(define connect PQconnectdb)
