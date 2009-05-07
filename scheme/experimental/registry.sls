#!r6rs
(library (experimental registry)
(export registry register)
(import (rnrs))

(define _registry '())

(define (registry)
  _registry)

(define (register id)
  (display "registering ") (display id) (newline)
  (set! _registry (append _registry (list id)))
  _registry)

(display "registry instantiated\n")
)
