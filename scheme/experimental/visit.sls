#!r6rs
(library (experimental visit)
(export a m)
(import (rnrs) (sweet-macros) (srfi :19))
(def-syntax m
  (begin
    (display "visiting at ")
    (display (date->string (current-date) "~5"))
    (newline)
    (lambda (x) #f)))
(define a 42)
(display "visit.sls instantiated!\n")
)
