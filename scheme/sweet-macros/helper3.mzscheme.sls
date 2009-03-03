#!r6rs
(library (sweet-macros)
(export local syntax-match def-syntax)
(import (rnrs) (for (sweet-macros helper2) run expand))

(define-syntax def-syntax
  (syntax-match (extends local)
    (sub (def-syntax name (extends parent)
       (local loc ...) (literal ...) 
       clause ...)
     #'(define-syntax name
         (syntax-match (local loc ...) (literal ...)
           clause ...
           (sub x ((parent <transformer>) #'x)))))
    (sub (def-syntax (name . args) skel . rest)
     #'(define-syntax name (syntax-match () (sub (name . args) skel . rest))))
    (sub (def-syntax name transformer)
     #'(define-syntax name transformer))
    ))
)
