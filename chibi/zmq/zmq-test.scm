(import (tizoc chibi zmq) (chibi test))

(test-assert "version"
             (equal? (list 2 0 10)
                     (zmq-version)))

(test-assert "init"
             (zmq-context? (zmq-init)))

(test-assert "with-zmq-context"
             (with-zmq-context
              (lambda (ctx)
                (zmq-context? ctx))))

(test-assert "socket init"
             (zmq-socket? (zmq-socket zmq-socket-type/req)))

(test
 "send/recv"
 '(#t "hola")
 (with-zmq-context
  (lambda (ctx)

    (let ((socket-req (zmq-socket zmq-socket-type/req ctx))
          (socket-rep (zmq-socket zmq-socket-type/rep ctx)))

      (zmq-bind socket-rep "inproc://test")
      (zmq-connect socket-req "inproc://test")

      (let* ((send-result (zmq-send-string socket-req "hola"))
             (recv-result (zmq-recv-string socket-rep)))

        (zmq-close socket-req)
        (zmq-close socket-rep)

        (list send-result recv-result))))))

(test-assert "select"
  (with-zmq-context
   (lambda (ctx)

     (let ((socket-req (zmq-socket zmq-socket-type/req ctx))
           (socket-rep (zmq-socket zmq-socket-type/rep ctx)))

       (zmq-bind socket-rep "inproc://test")
       (zmq-connect socket-req "inproc://test")

       (let* ((select1 (zmq-select (list socket-rep) (list socket-req)))
              (send-result (zmq-send-string socket-req "hola"))
              (select2 (zmq-select (list socket-rep) (list socket-req)))
              (recv-result (zmq-recv-string socket-rep)))

         (zmq-close socket-req)
         (zmq-close socket-rep)

         (and (equal? (list '() (list socket-req) '())
                      select1)
              (equal? (list (list socket-rep) '() '())
                      select2)))))))

(test
 "setsockopt and getsockopt"
 '(1 2 #t "test")
 (with-zmq-context
  (lambda (ctx)

    (let ((socket (zmq-socket zmq-socket-type/req ctx)))

      (zmq-setsockopt socket 'hwm 1)
      (zmq-setsockopt socket 'swap 2)
      (zmq-setsockopt socket 'mcast-loop #t)
      (zmq-setsockopt socket 'identity "test")

      (map (lambda (option) (zmq-getsockopt socket option))
           '(hwm swap mcast-loop identity))))))
