(define-library (tizoc clojure expression-threading)
  (export -> ->> doto)
  (import (scheme))
  (begin
    (define-syntax ->
      (syntax-rules ()
        ((_ ?form) ?form)
        ((_ ?form (?f ?arg ...)) (?f ?form ?arg ...))
        ((_ ?form ?f) (?f ?form))
        ((_ ?form ?form2 . ?forms) (-> (-> ?form ?form2) . ?forms))))

    (define-syntax ->>
      (syntax-rules ()
        ((_ ?form) ?form)
        ((_ ?form (?f ?arg ...)) (?f ?arg ... ?form))
        ((_ ?form ?f) (?f ?form))
        ((_ ?form ?form2 . ?forms) (->> (->> ?form ?form2) . ?forms))))

    (define-syntax doto
      (syntax-rules ()
        ((_ ?form) ?form)
        ((_ ?form (?f ?arg ...)) (?f ?form ?arg ...))
        ((_ ?form ?f) (?f ?form))
        ((_ ?form ?form2 . ?forms)
         (let ((value ?form))
           (doto value ?form2)
           (doto value . ?forms)
           value))))
    ))
