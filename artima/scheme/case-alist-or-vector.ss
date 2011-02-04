;; is case faster than alist? yes, nearly four times faster

(import (rnrs) (ikarus) (repeat-macro) (sweet-macros))

(define (alist a)
  (lambda (name)
    (let ((slot (assq name a)))
      (if slot (cadr slot) (error 'alist "NameError" name)))))

(define a (alist '((one 1) (two 2) (three 3) (four 4))))

(time (repeat 10000000 (a 'four)))

(def-syntax (case* (name value) ...)
  #'(lambda (n)
      (case n
        ((name) value) ...
        (else (error 'case* "NameError" n)))))

(define b (case* (one 1) (two 2) (three 3) (four 4)))

(time (repeat 10000000 (b 'three)))

(define (vec v)
  (lambda (i) (vector-ref v i)))

(define c (vec (vector 1 2 3 4)))

(time (repeat 10000000 (c 2)))

#|

Output:

running stats for (repeat 10000000 (a 'four)):
    no collections
    360 ms elapsed cpu time, including 0 ms collecting
    363 ms elapsed real time, including 0 ms collecting
    0 bytes allocated
running stats for (repeat 10000000 (b 'four)):
    no collections
    96 ms elapsed cpu time, including 0 ms collecting
    96 ms elapsed real time, including 0 ms collecting
    0 bytes allocated

|#
