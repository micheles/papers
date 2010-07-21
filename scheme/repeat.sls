(library (repeat)
  (export call)
  (import (rnrs))

  (define (call n proc . args)
    (let loop ((i 0))
      (when (< i n) (apply proc args) (loop (+ 1 i))))))
