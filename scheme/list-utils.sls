(library (list-utils)
 (export list-of fold range enumerate zip transpose distinct? perm
         remove-dupl merge-unique)
 (import (rnrs) (sweet-macros))

(define range 
  (case-lambda 
    ((n)
     (cond
      ((list? n) (range 0 (length n) 1))
      ((vector? n) (range 0 (vector-length n) 1))
      (else (range 0 n 1))))
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
   (list lst (range lst)))

 (define (zip . lists)
  (apply map list lists))

 (define (transpose llist)
  (apply map list llist))

 (def-syntax block
   (syntax-match (is)
    (sub (block)
        #'(void))
    (sub (block (name is value) rest ...)
        #'(let ((name value)) (block rest ...)))
    (sub (block rest ...)
        #'(begin rest ...))
    ))

 ;; ex. (fold right (cons x a) (a '()) (x in '(a b c))) sub (a b c)
 (def-syntax fold
   (syntax-match (left right in -> seed)
     (sub (fold left acc -> acc1 (item in items) ... (seed acc0))
      #'(fold-left (lambda (acc item ...) acc1) acc0 items ...)
      (for-all identifier? #'(acc item ...)))
     (sub (fold right acc -> acc1 (item in items) ... (seed acc0))
      #'(fold-right (lambda (item ... acc) acc1) acc0 items ...)
      (for-all identifier? #'(acc item ...)))
     (sub (fold acc -> acc1 (item in items) ...)
      #'(reverse (fold left acc -> acc1 (item in items) ... (seed '()))))
     ))

 ;; ex. (list-of (+ (* i 3) j) (i in (range 3)) (j in (range 3)))
(def-syntax _list-of
  (syntax-match (in is)
  
    (sub (_list-of expr acc)
     #'(cons expr acc))
  
    (sub (_list-of expr acc (var in lst) rest ...)
     #'(let loop ((ls lst))
         (if (null? ls) acc
             (let ((var (car ls)))
               (_list-of expr (loop (cdr ls)) rest ...)))))
   
    (sub (_list-of expr acc (var is exp) rest ...)
     #'(let ((var exp)) (_list-of expr acc rest ...)))
  
    (sub (_list-of expr acc pred? rest ...)
     #'(if pred? (_list-of expr acc rest ...) acc))
  ))

(def-syntax (list-of expr rest ...)
  #'(_list-of expr '() rest ...))

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
(define (perm lst)
  (cond
   ((null? lst) '()); empty list
   ((null? (cdr lst)) (list lst)); single element list
   (else; multi-element list 
    (list-of (cons el ls)
      (el in lst)
      (ls in (perm (list-of e (e in lst) (not (eq? e el)))))))))

;; ex: (remove-dupl '(1 2 3 1 5 2 4))
(define (remove-dupl eq? lst)
  (reverse
   (fold-left
    (lambda (acc el)
      (define (is-el? x) (eq? x el))
      (if (find is-el? acc); duplicate
          acc
          (cons el acc))) '() lst)))

(define (merge-unique eq? . lists)
  (remove-dupl eq? (apply append lists)))
)
