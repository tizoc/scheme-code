(define-library (tizoc chibi postgres-lowlevel)
  (export PQconnectdb
          PQsetdbLogin
          PQsetdb
          PQconnectStart
          PostgresPollingStatusType/FAILED
          PostgresPollingStatusType/READING
          PostgresPollingStatusType/WRITING
          PostgresPollingStatusType/OK
          PQconnectPoll
          PQreset
          PQresetStart
          PQresetPoll
          PQdb
          PQuser
          PQpass
          PQhost
          PQport
          PQtty
          PQoptions
          ConnStatusType/BAD
          ConnStatusType/OK
          ConnStatusType/STARTED
          ConnStatusType/MADE
          ConnStatusType/AWAITING_RESPONSE
          ConnStatusType/AUTH_OK
          ConnStatusType/SSL_STARTUP
          ConnStatusType/SETENV
          PQstatus
          PGTransactionStatusType/IDLE
          PGTransactionStatusType/ACTIVE
          PGTransactionStatusType/INTRANS
          PGTransactionStatusType/INERROR
          PGTransactionStatusType/UNKNOWN
          PQtransactionStatus
          PQparameterStatus
          PQprotocolVersion
          PQserverVersion
          PQerrorMessage
          PQsocket
          PQbackendPID
          PQconnectionNeedsPassword
          PQconnectionUsedPassword
          ExecStatusType/EMPTY_QUERY
          ExecStatusType/COMMAND_OK
          ExecStatusType/TUPLES_OK
          ExecStatusType/COPY_OUT
          ExecStatusType/COPY_IN
          ExecStatusType/BAD_RESPONSE
          ExecStatusType/NONFATAL_ERROR
          ExecStatusType/FATAL_ERROR
          ExecStatusType/COPY_BOTH
          PQexec
          PQexecParams/text
          PQresultStatus
          PQresStatus
          PQresultErrorMessage
          PQntuples
          PQnfields
          PQfname
          PQfnumber
          InvalidOid
          PQftable
          PQftablecol
          PQfformat
          PQftype
          PQfmod
          PQfsize
          PQgetvalue/text
          PQgetvalue/binary
          PQgetisnull
          PQgetlength
          PQnparams
          PQparamtype
          PQcmdStatus
          PQcmdTuples
          PQoidValue
          PQescapeLiteral
          PQescapeIdentifier
          PQsendQuery
          PQsendQueryParams/text
          PQgetResult
          PQconsumeInput
          PQisBusy
          PQsetnonblocking
          PQisnonblocking
          PQflush
          )

  (import (scheme base))
  (include-shared "postgres/postgres-lowlevel")

  (begin

    (define (PQgetvalue/binary results row col)
      (let* ((value (%PQgetvalue/binary results row col))
             (size (PQgetlength results row col))
             (bytes (make-bytevector size)))
        (%bytevector-fill-data bytes value size)
        bytes))

    (define (PQexecParams/text conn command params)
      (%PQexecParams/text conn command (length params) params))

    (define (PQsendQueryParams/text conn command params)
      (%PQsendQueryParams/text conn command (length params) params))

    (define (PQstring->string pqstring)
      (and pqstring
           (let ((string (c-string->string pqstring)))
             (PQfreemem pqstring)
             string)))

    (define (PQescapeLiteral conn str)
      (PQstring->string (%PQescapeLiteral conn str (string-bytes-count str))))

    (define (PQescapeIdentifier conn str)
      (PQstring->string (%PQescapeIdentifier conn str (string-bytes-count str))))

    ))
