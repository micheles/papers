#!r6rs
(library (experimental def-m1)
(export m1)
(import (rnrs) (sweet-macros) (for (only (experimental registry) register)
                                   expand))

(def-syntax m1
  (begin
    (register #'m1)
    (syntax-match () (sub (m1) #'1))))

(m1)
)
