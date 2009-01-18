(library (list-utils)
 (export list-of fold range enumerate zip transpose distinct? perm
         remove-dupl merge-unique)
 (import (rnrs) (sweet-macros) (list-match))

(define range 
  (case-lambda 
    ((n) (range 0 n 1))
    ((n0 n)
     (range n0 n 1))
    ((n0 n s)
     (begin
       (assert (and (for-all number? (list n0 n s)) (not (= s 0))))
       (let ((cmp (if (> s 0) >= <=)))
         (let loop ((i n0) (acc '()))
           (if (cmp i n) (reverse acc)
               (loop (+ i s) (cons i acc)))))))))
 
 (define (enumerate lst)
   (list (range (length lst)) lst))

 (define (zip . lists)
  (apply map list lists))

 (define (transpose llist)
  (apply map list llist))

 (def-syntax fold
   (syntax-match (left right in)
     (sub (fold left acc seed next (x ...) in llist)
          #'(apply fold-left (lambda (acc x ...) next) seed llist))
     (sub (fold right acc seed next (x ...) in llist)
          #'(apply fold-right (lambda (x ... acc) next) seed llist))
     ))

 ;; ex. (list-of (+ (* i 3) j) (i in (range 3)) (j in (range 3)))
(def-syntax list-of-aux
  (syntax-match (in is)
  
    (sub (list-of-aux expr acc)
     #'(cons expr acc))
  
    (sub (list-of-aux expr acc (var in lst) rest ...)
     #'(let loop ((ls lst))
         (if (null? ls) acc
             (let- ((var (car ls)))
               (list-of-aux expr (loop (cdr ls)) rest ...)))))
   
    (sub (list-of-aux expr acc (var is exp) rest ...)
     #'(let- ((var exp)) (list-of-aux expr acc rest ...)))
  
    (sub (list-of-aux expr acc pred? rest ...)
     #'(if pred? (list-of-aux expr acc rest ...) acc))
  ))

(def-syntax (list-of expr rest ...)
  #'(list-of-aux expr '() rest ...))

;; check if the elements of a list are distinct according to eq?
(define (distinct? eq? items)
  (define first (car items))
  (define rest (cdr items))
  (cond
   ((null? items) #t); no items
   ((null? rest) #t); single item
   ((exists (lambda (el) (eq? el first)) rest) #f)
   (else (distinct? eq? rest))))

;; compute the permutations of a list of distinct elements
(define (perm eq? lst)
  (cond
   ((null? lst) '()); empty list
   ((null? (cdr lst)) (list lst)); single element list
   (else; multi-element list 
    (list-of (cons el ls)
             (el in lst)
             (ls in (perm eq? (remp (lambda (e) (eq? el e)) lst)))))))

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
)
