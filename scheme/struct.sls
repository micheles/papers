(library (struct)
(export struct base-struct struct->alist struct-get)
(import (rnrs) (sweet-macros))

;; ex: (remove-dupl '(1 2 3 1 5 2 4))
(define (remove-dupl eq? lst)
  (reverse
   (fold-left
    (lambda (acc el)
      (define (is-el? x) (eq? x el))
      (if (find is-el? acc); duplicate
          acc
          (cons el acc))) '() lst)))

(define (append-unique eq? . lists)
  (remove-dupl eq? (apply append lists)))

(define (base-struct k)
  (case k
    ((->keys) '())
    (else (error 'struct-key-error "Missing key" k))))

(def-syntax struct
  (syntax-match ()
    (sub (struct (name value) ...)
         #'(struct base-struct (name value) ...))
    (sub (struct parent (name value) ...)
         #'(lambda (k)
             (case k
               ((->keys) (append-unique eq? '(name ...) (parent '->keys)))
               ((name) value) ...
               (else (parent k))))
         (for-all identifier? #'(name ...)))
    ))
             
(define (struct->alist s)
  (map (lambda (k) (list k (s k))) (s '->keys)))

(def-syntax (struct-get s name default)
  #'(let ((value (s 'name)))
      (if (eq? value struct-null) default value)))
)
