#!r6rs
;;; sweet-macros
;;; Version: 0.11
;;; Author: Michele Simionato
;;; Email: michele.simionato@gmail.com
;;; Date: 29-Oct-2008
;;; Licence: BSD

(library (sweet-macros helper1)
(export guarded-syntax-case syntax-match)
(import (rnrs))

;; helper macro
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
   (syntax-case x (sub)
    ((_ (literal ...) (sub patt skel . rest) ...)
     #'(lambda (x)
       (syntax-match x (literal ...)
         (sub patt skel . rest) ...)))
    ((_ x (literal ...) (sub patt skel . rest) ...)
     (and (identifier? #'x) (for-all identifier? #'(literal ...)))
     #'(guarded-syntax-case x
       (<literals> <patterns> <source> <transformer> literal ...)
       ((ctx <literals>)
        #''((... (... literal)) ...))
       ((ctx <patterns>)
        #''((... (... patt)) ...))
       ((ctx <source>)
        #''(syntax-match (literal ...)
              (... (... (sub patt skel . rest))) ...))
       ((ctx <transformer>)
        #'(syntax-match (literal ...)
              (... (... (sub patt skel . rest))) ...))
       (patt skel . rest) ...))
    )))
)
