(import (rnrs) (repeat-macro) (repeat) (only (ikarus) time))
(define n (string->number (car (reverse (command-line)))))
(time (call 10000000 + 1 n))
(time (repeat 10000000 (+ 1 n)))
