(library (aps compat)
(export printf
        (rename (ypsilon:format format)
                (ypsilon:gensym gensym)
                (ypsilon:pretty-print pretty-print)))
(import (rnrs) (prefix (core) ypsilon:))

(define (printf format-string . args)
  (display (apply ypsilon:format format-string args)))
)
