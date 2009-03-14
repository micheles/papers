#!r6rs
(library (sweet-macros)
(export syntax-match def-syntax)
(import (rnrs) (for (sweet-macros helper2) run expand))

(define-syntax def-syntax
  (syntax-match (extends)
    (sub (def-syntax name (extends parent) (literal ...) clause ...)
     #'(define-syntax name
         (syntax-match (literal ...)
           clause ...
           (sub x ((parent <transformer>) #'x)))))
    (sub (def-syntax (name . args) skel rest ...)
     #'(def-syntax name (syntax-match () (sub (name . args) skel rest ...)))
     (identifier? #'name) (syntax-violation 'def-syntax "Invalid name" #'name))
    (sub (def-syntax name transformer)
     #'(define-syntax name
         (syntax-match (<source> <transformer>)
           (sub (name <transformer>) #'(... (... transformer)))
           (sub (name <source>) #''(... (... transformer)))
           (sub x (transformer #'x))))
     (identifier? #'name) (syntax-violation 'def-syntax "Invalid name" #'name))
    ))
)
