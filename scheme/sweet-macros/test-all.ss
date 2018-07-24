#!r6rs
(import (rnrs) (aps compat) (sweet-macros) (aps list-utils)
        (aps easy-test) (for (only (aps list-utils) distinct?) expand))

;(def-syntax (quot x ...)
;  #''(x ...))

(def-syntax quot
  (syntax-match () (sub (quot x ...) #''(x ...))))

(display (syntax-expand (quot 1 2 3)))

(display (quot <patterns>))

(display "\nSuccess!\n")


(def-syntax (multi-define (name1 name2 ...) (value1 value2 ...))
  #'(begin (define name1 value1) (define name2 value2) ...)
  (distinct? bound-identifier=? #'(name1 name2 ...))
  (syntax-violation 'multi-define "Found duplicated in"
                    #'(name1 name2 ...)))

(run
 (test "md1"
       (multi-define <literals>)
       '())
 (test "md2"
       (multi-define <patterns>)
       '((multi-define (name1 name2 ...) (value1 value2 ...))))
 )
