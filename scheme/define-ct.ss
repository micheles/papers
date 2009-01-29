(import (rnrs) (sweet-macros) (table) (ikarus))

(def-syntax define-ct
  (syntax-match (define)
    (sub (define-ct kw (define name value) ...)
         #'(define-syntax kw
             (let ((t (tbl (name value) ...)))
               (syntax-match (name ...)
                (sub (kw name) (datum->syntax #'kw (t 'name))) ...))))))
(define-ct example
  (define x 1)
  (define y (* x 2)))

(pretty-print (syntax-expand
(define-ct example
  (define x 1)
  (define y (* x 2)))))

(display (list (example x) (example y)))
         
