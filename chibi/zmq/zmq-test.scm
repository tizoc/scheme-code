(import (tizoc chibi zmq) (chibi test))

(define (with-zmq-context body)
  (let* ((context (make-context (zmq-io-threads)))
         (result (body context)))
    (terminate-context context)
    result))

(test-assert "make-context"
             (context? (make-context 1)))

(test-assert "with-zmq-context"
             (with-zmq-context
              (lambda (ctx)
                (context? ctx))))

(test-assert "make-socket"
             (socket? (make-socket zmq-socket-type/req)))

(test
 "send/recv"
 '(#t "hola")
 (with-zmq-context
  (lambda (ctx)

    (let ((socket-req (make-socket zmq-socket-type/req ctx))
          (socket-rep (make-socket zmq-socket-type/rep ctx)))

      (bind-socket socket-rep "inproc://test")
      (connect-socket socket-req "inproc://test")

      (let* ((send-result (send-message socket-req "hola"))
             (recv-result (receive-message socket-rep)))

        (close-socket socket-req)
        (close-socket socket-rep)

        (list send-result recv-result))))))

(test-assert "select"
  (with-zmq-context
   (lambda (ctx)

     (let ((socket-req (make-socket zmq-socket-type/req ctx))
           (socket-rep (make-socket zmq-socket-type/rep ctx)))

       (bind-socket socket-rep "inproc://test")
       (connect-socket socket-req "inproc://test")

       (let* ((select1 (select (list socket-rep) (list socket-req)))
              (send-result (send-message socket-req "hola"))
              (select2 (select (list socket-rep) (list socket-req)))
              (recv-result (receive-message socket-rep)))

         (close-socket socket-req)
         (close-socket socket-rep)

         (and (equal? (list '() (list socket-req) '())
                      select1)
              (equal? (list (list socket-rep) '() '())
                      select2)))))))

(test
 "setsockopt and getsockopt"
 '(1 2 #t "test")
 (with-zmq-context
  (lambda (ctx)

    (let ((socket (make-socket zmq-socket-type/req ctx)))

      (socket-option-set! socket 'hwm 1)
      (socket-option-set! socket 'swap 2)
      (socket-option-set! socket 'mcast-loop #t)
      (socket-option-set! socket 'identity "test")

      (map (lambda (option) (socket-option socket option))
           '(hwm swap mcast-loop identity))))))
