#!r6rs
(library (experimental mod2)
  (export get-x incr-x)
  (import (rnrs))
  
  (define x 0)

  (define (get-x)
    x)
  
  (define (incr-x)
    (set! x (+ 1 x))
    x)

  (display "Instantiated mod2\n")
)
