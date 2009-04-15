#!r6rs
(library (experimental registry)
(export registry register)
(import (rnrs) (aps compat))

(define _registry '())

(define (registry)
  _registry)

(define (register id)
  (printf "registering ~a\n" id)
  (set! _registry (append _registry (list id)))
  _registry)
)
