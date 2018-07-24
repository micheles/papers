(library (aps combinator)
;;; Version: 0.1
;;; Author: Michele Simionato
;;; Email: michele.simionato@gmail.com
;;; Date: 1-Feb-2009
;;; Licence: BSD
(export compose-left compose-right str)
(import (rnrs) (sweet-macros))

;;COMPOSE-LEFT
;; f1 f2 -> f2 o f1
(define (compose-left . functions)
  (lambda (x)
    (fold-left (lambda (a f) (f a)) x functions)))
;;END

;;COMPOSE-RIGHT
;; f1 f2 -> f1 o f2
(define (compose-right . functions)
  (lambda (x)
    (fold-right (lambda (f a) (f a)) x functions)))
;;END

;;STR
(define (str s)
  ;; return a one-argument function appending the string s on the left
  (lambda (x) (string-append s x)))
;;END

)
