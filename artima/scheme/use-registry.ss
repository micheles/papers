#!r6rs
(import (rnrs) (sweet-macros) (only (aps compat) printf)
        (for (only (experimental registry) register registry) expand)
        (for (only (experimental registry) registry) run)
        (experimental def-m1))

(def-syntax (define-registered name)
  #`(define name '#,(registry)))

(def-syntax m2
  (begin
    (register #'m2)
    (syntax-match () (sub (m2) #'2))))

(m2)
(define-registered names)
(display "Registered: ")
(display names)
(newline)
(printf "Registered ~a macro(s)\n" (length (registry)))
