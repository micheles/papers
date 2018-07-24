#!r6rs
(library (aps define-ct)
(export compilation-time)
(import (rnrs) (sweet-macros) (aps lang) (aps list-utils) (srfi :19))

;; see http://srfi.schemers.org/srfi-19/srfi-19.html
(define-syntax compilation-time
  (let ((isodate (date->string (current-date) "~5")));; visit time
    (display "Visit time: ") (display isodate) (newline)
    (lambda (x) isodate)))

;;DEFINE-CT
;(def-syntax (define-ct kw (define name value) ...)
;  #'(def-syntax kw
;      (let ((a (alist (name value) ...)))
;        (syntax-match (name ...)
;          (sub (kw name) (datum->syntax #'kw (car (assq 'name a))))
;          ...)))
;  (eq? (syntax->datum #'define) 'define))
;;END

)
