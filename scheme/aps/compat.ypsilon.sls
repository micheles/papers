(library (aps compat)
(export printf format gensym pretty-print)))
(import (rnrs) (core))

(define (printf format-string . args)
  (display (apply format format-string args)))
)
