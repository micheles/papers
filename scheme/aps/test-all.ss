(import (rnrs) (aps easy-test) (aps list-utils))

(define (test-range-zip-transpose-enumerate)
  (list
   (test "range1"
         (range 3)
         '(0 1 2))
   
   (test "range2"
         (range 0 3)
         '(0 1 2))
   
   (test "range3"
         (range 6 0 -2)
         '(6 4 2))
   
   (test "zip"
         (zip '(a b c) '(1 2 3))
         '((a 1) (b 2) (c 3)))

   ;;ZIP-WITH-ERROR
   (test "zip-with-error"
         (catch-error (zip '(a b c) '(1 2)))
         "length mismatch")
   ;;END
   
   (test "zip3"
         (zip '(a b) '(1 2) '(X Y))
         '((a 1 X) (b 2 Y)))

   (test "transpose"
         (transpose '((x y) (1 2)))
         '((x 1) (y 2)))
   
   (test "transpose-error"
         (catch-error (transpose '((x y) 1)))
         "not a list of lists")

   (test "enumerate"
         (enumerate '(a b c))
         '((0 a) (1 b) (2 c)))
   ))

(define (test-distinct)
  (define (eq1? a b)
    (eq? (car a) (car b)))
  (list
   (test "distinct-true"
         (distinct? eq? '(a b c))
         #t)
   
   (test "distinct-false"
         (distinct? eq? '(a b c c))
         #f)
   
   (test "distinct-alist"
         (distinct? eq1? '((a 1) (b 2) (a 3)))
         #f)
   
   (test "remove-dupl"
         (remove-dupl eq1? '((a 1) (b 2) (a 3)))
         '((a 1) (b 2)))

   (test "append-unique-1"
         (append-unique eq? '(1 2 3) '(1 5 2 4))
         '(1 2 3 5 4))
   
   (test "append-unique-2"
         (append-unique eq1? '((a 1) (b 2) (a 3)) '((d 4)) '((b 5) (d 2)))
         '((a 1) (b 2) (d 4)))

   (test "perm-1"
         (perm eq? '(1 2 3))
         '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))
   
   (test "perm-2"
         (perm eq1? '((a 1) (b 2)))
         '(((a 1) (b 2)) ((b 2) (a 1))))
   
   (test "perm-with-dupl"
         (perm eq? '(a b a c))
         '((a b c) (a c b) (b a c) (b a c) (a b c) (a c b) (c a b) (c a b)))
   
   (test "flatten-empty"
         (flatten '())
         '())

   (test "flatten-flat"
         (flatten '(1 2 3))
         '(1 2 3))

   (test "flatten-nested"
         (flatten '(1 (2 3) (4 (5 6 (7 8)))))
         '(1 2 3 4 5 6 7 8))
   ))

(define (test-let+comprehension)
  (list
   (test "let+1"
         (let+ (x 1) x)
         1)
   
   (test "let+2"
         (let+ ((x y) '(1 2)) y)
         2)
   
   (test "let+3"
         (let+ ((x (y z)) '(1 (2 3))) z)
         3)
   
   (test "list-of"
         (list-of (cons i x) ((i x) in (enumerate '(a b c))) (even? i))
         '((0 . a) (2 . c)))

   ))
   
(apply run (append (test-range-zip-transpose-enumerate)
                   (test-distinct)
                   (test-let+comprehension)
                   ))
   
