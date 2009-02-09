(library (aps struct)
(export struct base struct->alist)
(import (rnrs) (sweet-macros) (aps list-utils))

;;STRUCT
(def-syntax (struct base (name value) ...)
  (begin
    (assert (for-all identifier? #'(name ...)))
    (assert (distinct? eq? #'(name ...)))
    #'(letrec ((->names (append-unique eq? '(name ...) (base '->names)))
               (name value) ...)
        (lambda (n)
          (case n
            ((name) name) ...
            ((->names) ->names)
            ((->base) base)
            (else (base n)))))))
;;END

;;BASE-STRUCT
(define (base n)
  (case n
    ((->names) '())
    ((->base) #f)
    (else (error 'struct "Unknown name" n))))
;;END

;;STRUCT->ALIST
(define (struct->alist s)
  (map (lambda (k) (list k (s k))) (s '->names)))
;;END
)
