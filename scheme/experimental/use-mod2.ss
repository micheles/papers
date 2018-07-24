(import (rnrs) (sweet-macros) (for (experimental mod2) expand run))

(def-syntax m
  (lambda (x)
    (display "At expand-time x=")
    (display (incr-x))
    (newline)
    "m-expanded"))

(m)

(begin
  (display "At run-time x=")
  (display (incr-x))
  (newline))
