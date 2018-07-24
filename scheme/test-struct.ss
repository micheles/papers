(import (rnrs) (struct) (sweet-macros) (ikarus))

;(display (syntax-expand (struct (a 1) (b 2))))

;(pretty-print (syntax-expand (struct base-struct (a 1) (b 2))))

(define s1 (struct (a 1) (b 2)))
(define s2 (struct s1 (c 3)))
(define s3 (struct s2 (a 4)))

(define (struct->alist s)
  (map (lambda (k) (list k (s k))) (s '->keys)))

(display (struct->alist s3))

(for-each (lambda (k) (display (s3 k))) (s3 '->keys))
