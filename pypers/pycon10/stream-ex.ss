(import (rnrs) (srfi :41 streams))

(define str123 (stream-range 1 4))

(stream-for-each display str123)

(stream-for-each display str123)
