#!r6rs
(import (rnrs) (sweet-macros)
        (for (only (experimental registry) register) expand)
        (for (only (experimental registry) registry) run))

(def-syntax m
  (begin
    (register #'m)
    (syntax-match () (sub (m) #'42))))

(m)
(display (registry))
