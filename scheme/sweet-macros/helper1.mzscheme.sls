#!r6rs
(library (sweet-macros helper1)
(export guarded-syntax-case)
(import (rnrs))

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
        ((guarded-syntax-case y (literal ...) clause ...)
         (with-syntax (((c ...) (fold-right add-clause '() #'(clause ...))))
           #'(syntax-case y (literal ...) c ...)))))))
)
