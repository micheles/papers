(import (rnrs) (experimental define+) (experimental defines))
(visit-defines)
(define-registered names)
(display names)
(display (list a b));; just to use the names defined in defines
