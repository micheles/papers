(import (rnrs) (sweet-macros) (experimental registry))

(def-syntax m
  (begin
    (register #'m)
    (syntax-match () (sub (m) #'42))))

(m)
(display (registry))
