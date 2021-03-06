#!r6rs
(library (experimental mod3)
(export run)
(import (rnrs) (sweet-macros) (for (experimental mod2) expand run))

(def-syntax m
  (lambda (x)
    (display "At expand-time x=")
    (display (incr-x))
    (newline)
    "m-expanded"))

(define (run) ;; this is executed at runtime
  (display "At run-time x=")
  (display (incr-x))
  (newline))

(m) ;; this is executed at expand time
)

