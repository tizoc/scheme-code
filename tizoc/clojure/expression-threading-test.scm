(import (scheme) (tizoc clojure expression-threading) (chibi test))

(test
 "(-> 3.0 (- 2) (* 5) sqrt)"
 (sqrt (* (- 3.0 2) 5))
 (-> 3.0 (- 2) (* 5) sqrt))

(test
 "(->> 3.0 (- 5) (* 5) sqrt)"
 (sqrt (* 5 (- 5 3.0)))
 (->> 3.0 (- 5) (* 5) sqrt))

(test
 "(doto (make-vector 3) (vector-set! 0 0) (vector-set! 1 1) (vector-set! 2 2))"
 (vector 0 1 2)
 (doto (make-vector 3) (vector-set! 0 0) (vector-set! 1 1) (vector-set! 2 2)))
