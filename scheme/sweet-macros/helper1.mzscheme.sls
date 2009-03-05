#!r6rs
(library (sweet-macros helper1)
(export locally guarded-syntax-case)
(import (rnrs))

(define-syntax locally
  (lambda (x)
    (syntax-case x (syntax-match)
      ((locally expr)
       #'expr)
      ((locally (let-form name value) ... (syntax-match b0 b1 b2 ...))
       #'(syntax-match (locally (let-form name value) ...) b0 b1 b2 ...))
      ((locally (let-form name value) (l n v) ... expr)
       #'(let-form ((name value)) (locally (l n v) ... expr))))
    ))

(define-syntax guarded-syntax-case
  (let ((add-clause
         (lambda (clause acc)
           (syntax-case clause ()
             ((pattern skeleton . rest)
                (syntax-case #'rest ()
                  ((cond? else1 else2 ...)
                   (cons*
                    #'(pattern cond? skeleton)
                    #'(pattern (begin else1 else2 ...))
                    acc))
                  ((cond?)
                   (cons #'(pattern cond? skeleton) acc))
                  (()
                   (cons #'(pattern skeleton) acc))
                  ))))))
    (lambda (x)
      (syntax-case x ()
        ((guarded-syntax-case () (literal ...) clause ...)
         #'(lambda (y) (guarded-syntax-case y (literal ...) clause ...)))
        ((guarded-syntax-case y (literal ...) clause ...)
         (with-syntax
             (((c ...) (fold-right add-clause '() #'(clause ...))))
           #'(syntax-case y (literal ...) c ...)))
        ))))
)
