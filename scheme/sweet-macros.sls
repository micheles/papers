(library (sweet-macros)
(export syntax-match def-syntax syntax-expand)
(import (rnrs) (ikarus))

;; helper         
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
         (with-syntax
             (((c ...) (fold-right add-clause '() #'(clause ...))))
           #'(syntax-case y (literal ...) c ...)))))))

(define-syntax syntax-match
  (lambda (x)
   (syntax-case x (=>)
    ((_ (literal ...) (=> patt skel . rest) ...)
     #'(lambda (x)
       (syntax-match x (literal ...)
         (=> patt skel . rest) ...)))
    ((_ x (literal ...) (=> patt skel . rest) ...)
     (and (identifier? #'x) (for-all identifier? #'(literal ...)))
     #'(guarded-syntax-case x
       (<literals> <patterns> <source> <transformer> literal ...)
       ((ctx <literals>)
        #''((... (... literal)) ...))
       ((ctx <patterns>)
        #''((... (... patt)) ...))
       ((ctx <source>)
        #''(syntax-match (literal ...)
              (... (... (=> patt skel . rest))) ...))
       ((ctx <transformer>)
        #'(syntax-match (literal ...)
              (... (... (=> patt skel . rest))) ...))
       (patt skel . rest) ...))
    )))

(define-syntax def-syntax
  (syntax-match ()
    (=> (def-syntax (name . args) skel . rest)
     #'(define-syntax name (syntax-match () (=> (name . args) skel . rest))))
    (=> (def-syntax name transformer)
     #'(define-syntax name transformer))
    ))

(def-syntax (syntax-expand (macro . args))
  #'(syntax->datum ((macro <transformer>) '(macro . args))))

)
