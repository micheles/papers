#!r6rs
(library (sweet-macros helper2)
(export syntax-match)
(import (rnrs) (for (rnrs) (meta -1))
(for (sweet-macros helper1) (meta -1) (meta 0) (meta 1)))

(define-syntax syntax-match
  (lambda (y)
    (guarded-syntax-case y (sub)
      
      ((self (literal ...) (sub patt skel rest ...) ...)
       #'(lambda (x) (self x (literal ...) (sub patt skel rest ...) ...)))
      
      ((self x (literal ...) (sub patt skel rest ...) ...)
       #'(guarded-syntax-case x (<literals> <patterns> literal ...)
           ((ctx <literals>) #''(literal ...))
           ((ctx <patterns>) #''((... (... patt)) ...))
           (patt skel rest ...)
           ...)
       (for-all identifier? #'(literal ...))
       (syntax-violation 'syntax-match "Found non identifier" #'(literal ...)
                         (remp identifier? #'(literal ...))))
      )))
)
