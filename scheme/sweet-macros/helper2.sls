#!r6rs

(library (sweet-macros helper2)
(export def-syntax syntax-match)
(import (rnrs) (for (sweet-macros helper1) run expand))

(define-syntax def-syntax
  (syntax-match ()
    (sub (def-syntax (name . args) skel . rest)
     #'(define-syntax name (syntax-match () (sub (name . args) skel . rest))))
    (sub (def-syntax name transformer)
     #'(define-syntax name transformer))
    ))
)
