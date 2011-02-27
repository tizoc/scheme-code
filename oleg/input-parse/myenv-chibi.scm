;; Fragments extracted from myenv-chicken.scm of chicken's SSAX egg

; Frequently-occurring syntax-rule macros

; A symbol? predicate at the macro-expand time
;	symbol?? FORM KT KF
; FORM is an arbitrary form or datum
; expands in KT if FORM is a symbol (identifier), Otherwise, expands in KF

(define-syntax symbol??
  (syntax-rules ()
    ((symbol?? (x . y) kt kf) kf)	; It's a pair, not a symbol
    ((symbol?? #(x ...) kt kf) kf)	; It's a vector, not a symbol
    ((symbol?? maybe-symbol kt kf)
      (let-syntax
	((test
	   (syntax-rules ()
	     ((test maybe-symbol t f) t)
	     ((test x t f) f))))
	(test abracadabra kt kf)))))

; A macro-expand-time memv function for identifiers
;	id-memv?? FORM (ID ...) KT KF
; FORM is an arbitrary form or datum, ID is an identifier.
; The macro expands into KT if FORM is an identifier, which occurs
; in the list of identifiers supplied by the second argument.
; All the identifiers in that list must be unique.
; Otherwise, id-memv?? expands to KF.
; Two identifiers match if both refer to the same binding occurrence, or
; (both are undefined and have the same spelling).

; (id-memv??			; old code. 
;   (syntax-rules ()
;     ((_ x () kt kf) kf)
;     ((_ x (y . rest) kt kf)
;       (let-syntax
; 	((test 
; 	   (syntax-rules (y)
; 	     ((test y _x _rest _kt _kf) _kt)
; 	     ((test any _x _rest _kt _kf)
; 	       (id-memv?? _x _rest _kt _kf)))))
; 	(test x x rest kt kf)))))


(define-syntax id-memv??
  (syntax-rules ()
    ((id-memv?? form (id ...) kt kf)
      (let-syntax
	((test
	   (syntax-rules (id ...)
	     ((test id _kt _kf) _kt) ...
	     ((test otherwise _kt _kf) _kf))))
	(test form kt kf)))))

; Test cases
; (id-memv?? x (a b c) #t #f)
; (id-memv?? a (a b c) 'OK #f)
; (id-memv?? () (a b c) #t #f)
; (id-memv?? (x ...) (a b c) #t #f)
; (id-memv?? "abc" (a b c) #t #f)
; (id-memv?? x () #t #f)
; (let ((x 1))
;   (id-memv?? x (a b x) 'OK #f))
; (let ((x 1))
;   (id-memv?? x (a x b) 'OK #f))
; (let ((x 1))
;   (id-memv?? x (x a b) 'OK #f))

; Commonly-used CPS macros
; The following macros follow the convention that a continuation argument
; has the form (k-head ! args ...)
; where ! is a dedicated symbol (placeholder).
; When a CPS macro invokes its continuation, it expands into
; (k-head value args ...)
; To distinguish such calling conventions, we prefix the names of
; such macros with k!

(define-syntax k!id			; Just the identity. Useful in CPS
  (syntax-rules ()
    ((k!id x) x)))

; k!reverse ACC (FORM ...) K
; reverses the second argument, appends it to the first and passes
; the result to K

(define-syntax k!reverse
  (syntax-rules (!)
    ((k!reverse acc () (k-head ! . k-args))
      (k-head acc . k-args))
    ((k!reverse acc (x . rest) k)
      (k!reverse (x . acc) rest k))))


; (k!reverse () (1 2 () (4 5)) '!) ;==> '((4 5) () 2 1)
; (k!reverse (x) (1 2 () (4 5)) '!) ;==> '((4 5) () 2 1 x)
; (k!reverse (x) () '!) ;==> '(x)

; Some useful increment/decrement (aka predecessor/successor) operators

				; Mutable increment
(define-syntax inc!
  (syntax-rules () ((inc! x) (set! x (+ 1 x)))))

				; Read-only increment
(define-syntax inc
  (syntax-rules () ((inc x) (+ 1 x))))

				; Mutable decrement
(define-syntax dec!
  (syntax-rules () ((dec! x) (set! x (- x 1)))))

				; Read-only decrement
(define-syntax dec
  (syntax-rules () ((dec x) (- x 1))))

			; Is str the empty string?
			; string-null? str -> bool
			; See Olin Shiver's Underground String functions
(define-syntax string-null?
  (syntax-rules ()
    ((string-null? str) (zero? (string-length str)))))

; A rather useful utility from SRFI-1
; cons* elt1 elt2 ... -> object
;    Like LIST, but the last argument provides the tail of the constructed
;    list -- i.e., (cons* a1 a2 ... an) = (cons a1 (cons a2 (cons ... an))).
;
;   (cons* 1 2 3 4) => (1 2 3 . 4)
;   (cons* 1) => 1
(define (cons* first . rest)
  (let recur ((x first) (rest rest))
    (if (pair? rest)
	(cons x (recur (car rest) (cdr rest)))
	x)))

(define (identify-error msg args . disposition-msgs)
  (let ((port (current-error-port)))
    (newline port)
    (display "ERROR" port)
    (display msg port)
    (for-each (lambda (msg) (display msg port))
	      (append args disposition-msgs))
    (newline port)))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () . bodies) (begin . bodies))
    ((let*-values (((var) initializer) . rest) . bodies)
      (let ((var initializer))		; a single var optimization
	(let*-values rest . bodies)))
    ((let*-values ((vars initializer) . rest) . bodies)
      (call-with-values (lambda () initializer) ; the most generic case
	(lambda vars (let*-values rest . bodies))))))

;
; syntax: assert ?expr ?expr ... [report: ?r-exp ?r-exp ...]
;
; If (and ?expr ?expr ...) evaluates to anything but #f, the result
; is the value of that expression.
; If (and ?expr ?expr ...) evaluates to #f, an error is reported.
; The error message will show the failed expressions, as well
; as the values of selected variables (or expressions, in general).
; The user may explicitly specify the expressions whose
; values are to be printed upon assertion failure -- as ?r-exp that
; follow the identifier 'report:'
; Typically, ?r-exp is either a variable or a string constant.
; If the user specified no ?r-exp, the values of variables that are
; referenced in ?expr will be printed upon the assertion failure.

(define-syntax assert
  (syntax-rules (report:)
    ((assert "doit" (expr ...) (r-exp ...))
     (cond
      ((and expr ...) => (lambda (x) x))
      (else
       (error "assertion failure: ~a" (list '(and expr ...) r-exp ...)))))
    ((assert "collect" (expr ...))
     (assert "doit" (expr ...) ()))
    ((assert "collect" (expr ...) report: r-exp ...)
     (assert "doit" (expr ...) (r-exp ...)))
    ((assert "collect" (expr ...) expr1 stuff ...)
     (assert "collect" (expr ... expr1) stuff ...))
    ((assert stuff ...)
     (assert "collect" () stuff ...))))

(define-syntax assure
  (syntax-rules ()
    ((assure exp error-msg)
     (assert exp report: error-msg))))

