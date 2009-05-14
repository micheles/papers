#!r6rs
(library (experimental L)
(export m a)
(import (rnrs) (sweet-macros))
(def-syntax m
  (begin
    (display "visiting L\n")
    (lambda (x) #f)))
(define a 42)
(display "L instantiated\n")
)
