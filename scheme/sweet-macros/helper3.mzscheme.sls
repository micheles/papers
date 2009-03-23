#!r6rs
(library (sweet-macros)
(export syntax-match def-syntax)
(import (rnrs) (for (sweet-macros helper2) run expand))

(define-syntax def-syntax
  (syntax-match (extends)

    (sub (def-syntax (name . args) skel rest ...)
     #'(def-syntax name (syntax-match () (sub (name . args) skel rest ...))))
    
    (sub (def-syntax name transformer)
     #'(define-syntax name
         (lambda (x)
           (syntax-case x (<source> <transformer>)
             ((name <transformer>) #'(... (... transformer)))
             ((name <source>) #''(... (... transformer)))
             (x (transformer #'x)))))
     (identifier? #'name))
     ;(syntax-violation 'def-syntax "Invalid name" #'name))

    (sub (def-syntax name (extends parent) (literal ...) clause ...)
     #'(def-syntax name
         (syntax-match (literal ...)
           clause ...
           (sub x ((parent <transformer>) #'x)))))
    ))
)
