#!r6rs
(library (experimental defines)
(export a b visit-defines)
(import (rnrs) (experimental define+))

(define-syntax visit-defines
  (lambda (x) "visited"))

(define+ a 1)
(define+ b 2)

(display "defines instantiated\n")
)

