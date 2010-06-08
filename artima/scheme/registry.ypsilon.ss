(import (rnrs) (sweet-macros))

(define registry '())

(define (register macro-id)
  (set! registry (append registry (list macro-id))))

(def-syntax m
  (begin
    (register #'m)
    (syntax-match () (sub (m) #'42))))

(m)

(display "registry: ") (display registry)
