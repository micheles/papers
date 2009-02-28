#!r6rs
(library (sweet-macros helper2)
(export local guarded-syntax-case syntax-match)
(import (rnrs) (for (sweet-macros helper1) run expand))

(define-syntax syntax-match
  (guarded-syntax-case () (sub local)
    ((self (local (let-form name value) ...) (literal ...)
           (sub patt skel . rest) ...)
     #'(local (let-form name value) ...
         (guarded-syntax-case ()
           (<literals> <patterns> <source> <transformer> literal ...)
           ((ctx <literals>)
            #''((... (... literal)) ...))
           ((ctx <patterns>)
            #''((... (... patt)) ...))
           ((ctx <source>)
            #''(self (local (let-form name value) ...) ((... (... literal)) ...)
                     (... (... (sub patt skel . rest))) ...))
           ((ctx <transformer>)
            #'(self (local (let-form name value) ...) ((... (... literal)) ...)
                    (... (... (sub patt skel . rest))) ...))
           (patt skel . rest) ...))
     (for-all identifier? #'(literal ...))
     (syntax-violation 'syntax-match "Found non identifier" #'(literal ...)
                       (remp identifier? #'(literal ...))))
    
    ((self (literal ...) (sub patt skel . rest) ...)
     #'(self (local)(literal ...) (sub patt skel . rest) ...))

    ((self x (literal ...) (sub patt skel . rest) ...)
     #'(guarded-syntax-case x (literal ...) (patt skel . rest) ...))
    ))
)
