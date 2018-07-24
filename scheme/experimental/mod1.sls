#!r6rs
(library (experimental mod1)
  (export x incr-x)
  (import (rnrs))
  
  (define x 0)
  (define (incr-x)
    (set! x (+ 1 x))
    x)
)
