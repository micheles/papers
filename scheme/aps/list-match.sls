(library (aps list-match)
;;; Version: 0.2
;;; Author: Michele Simionato
;;; Email: michele.simionato@gmail.com
;;; Date: 31-Jan-2009
;;; Licence: BSD
(export list-match)
(import (rnrs) (sweet-macros))

(def-syntax list-match-aux
  (syntax-match (quote quasiquote)
      (sub (_ obj pattern action)
           #'(list-match-aux obj pattern action #t))
      (sub (_ obj () action guard)
           #'(and (null? obj) guard action))
      (sub (_ obj underscore action guard)
           #'(and guard action)
           (and (identifier? #'underscore)(free-identifier=? #'underscore #'_)))
      (sub (_ obj var action guard)
           #'(let ((var obj)) (and guard action))
        (identifier? #'var))
      (sub (_ obj (quote datum) action guard)
           #'(and (equal? obj (quote datum)) guard action))
      (sub (_ obj (quasiquote datum) action guard)
           #'(and (equal? obj (quasiquote datum)) guard action))
      (sub (_ obj (kar . kdr) action guard)
           #'(and (pair? obj)
                  (let ((kar-obj (car obj)) (kdr-obj (cdr obj)))
                    (list-match-aux kar-obj kar
                      (list-match-aux kdr-obj kdr action guard)))))
      (sub (_ obj const action guard)
           #'(and (equal? obj const) guard action))
      ))

(def-syntax list-match
  (syntax-match (when)
    (sub (list-match lst (when pattern action guard ...) ...)
      #'(let ((ls lst))
          (cond
           ((list-match-aux ls pattern (list action) guard ...) => car) ...
           (else (error 'list-match "pattern mismatch" ls)))))))
)
