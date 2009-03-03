#!r6rs
(library (sweet-macros helper1)
(export local guarded-syntax-case)
(import (rnrs))

(define-syntax local
  (lambda (x)
    (syntax-case x (syntax-match)
      ((local expr)
       #'expr)
      ((local (let-form name value) ... (syntax-match b0 b1 b2 ...))
       #'(syntax-match (local (let-form name value) ...) b0 b1 b2 ...))
      ((local (let-form name value) (l n v) ... expr)
       #'(let-form ((name value)) (local (l n v) ... expr))))
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
