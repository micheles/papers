#!r6rs
(library (aps list-utils)
(export range enumerate zip transpose distinct? let+ perm list-of-aux list-for
        remove-dupl append-unique deep-map deep-fold fold
        flatten list-of normalize symbol-table)
(import (rnrs) (sweet-macros) (aps cut) (for (aps lang) expand))

;;; macros

;;LET+
(def-syntax let+
  (syntax-match ()
    (sub (let+ expr)
         #'expr)
    (sub (let+ (() lst) expr)
         #'(if (null? lst) expr
               (apply error 'let+ "Too many elements" lst)))
    (sub (let+ ((arg1 arg2 ... . rest) lst) expr)
         #'(let ((ls lst))
             (if (null? ls)
                 (apply error 'let+ "Missing arguments" '(arg1 arg2 ...))
                 (let+ (arg1 (car ls))
                   (let+ ((arg2 ... . rest) (cdr ls)) expr)))))
    (sub (let+ (name value) expr)
         #'(let ((name value)) expr)
         (identifier? #'name)
         (syntax-violation 'let+ "Argument is not an identifier" #'name))
    (sub (let+ (name value) (n v) ... expr)
         #'(let+ (name value) (let+ (n v) ... expr)))
    ))
;;END

;;DEEP-MAP
(define (deep-map f lst)
  (map (lambda (x)
         (if (and (list? x) (not (eq? 'quote (car x))))
             (deep-map f x)
             (f x))) lst))
;;END

;;DEEP-FOLD
(define (deep-fold f seed lst);; this is flattening the structure
  (fold-right
   (lambda (x a)
     (if (and (list? x) (not (eq? 'quote (car x))))
         (deep-fold f a x) (f x a))) seed lst))
;;END

;;LIST-OF

(def-syntax list-of-aux
  (syntax-match (in is)
  
    (sub (list-of-aux expr acc)
     #'(cons expr acc))
  
    (sub (list-of-aux expr acc (var in lst) rest ...)
     #'(let loop ((ls lst))
         (if (null? ls) acc
             (let+ (var (car ls))
               (list-of-aux expr (loop (cdr ls)) rest ...)))))
   
    (sub (list-of-aux expr acc (var is exp) rest ...)
     #'(let+ (var exp) (list-of-aux expr acc rest ...)))
  
    (sub (list-of-aux expr acc pred? rest ...)
     #'(if pred? (list-of-aux expr acc rest ...) acc))
  ))

(def-syntax (list-of expr rest ...)
  #'(list-of-aux expr '() rest ...))

;;END

(def-syntax (list-for decl ... expr)
  #'(list-of-aux expr '() decl ...))

;;SYMBOL-TABLE
(def-syntax (symbol-table arg ...)
  (: with-syntax
     ((name value) ...)
     (map (syntax-match ()
            (sub n #'(n n) (identifier? #'n))
            (sub (n v) #'(n v) (identifier? #'n)))
          #'(arg ...))
     #'(letrec ((name value) ...)
         (list (list 'name name) ...))))
;;END

;;; utilities

;;RANGE
(define range 
  (case-lambda 
    ((n) (range 0 n 1))
    ((n0 n)
     (range n0 n 1))
    ((n0 n s)
     (begin
       (assert (and (for-all number? (list n0 n s)) (not (zero? s))))
       (let ((cmp (if (> s 0) >= <=)))
         (let loop ((i n0) (acc '()))
           (if (cmp i n) (reverse acc)
               (loop (+ i s) (cons i acc)))))))))
;;END

;;ZIP
(define (zip . lists)
  (assert (for-all list? lists))
  (apply map list lists))
;;END

;;TRANSPOSE
 (define (transpose llist)
   (if (and (list? llist) (for-all list? llist))
       (apply map list llist)
       (error 'transpose "not a list of lists" llist)))
;;END

;;ENUMERATE
(define (enumerate lst)
  (zip (range (length lst)) lst))
;;END

;;DISTINCT?
;; check if the elements of a list are distinct according to eq?
(define (distinct? eq? items)
  (if (null? items) #t ; no items
      (let+ ((first . rest) items)
        (cond
         ((null? rest) #t); single item
         ((exists (cut eq? first <>) rest) #f); duplicate
         (else (distinct? eq? rest)); look at the sublist
         ))))
;;END

;;REMOVE-DUPL
;; ex: (remove-dupl = '(1 2 3 1 5 2 4)) => (1 2 3 5 4)
(define (remove-dupl eq? lst)
  (reverse
   (fold-left
    (lambda (acc el)
      (if (exists (cut eq? <> el) acc); duplicate
          acc
          (cons el acc)))
    '() lst)))
;;END

;;APPEND-UNIQUE
;; ex: (append-unique = '(1 2 3) '(1 5 2 4)) => (1 2 3 5 4)
(define (append-unique eq? . lists)
  (remove-dupl eq? (apply append lists)))
;;END

;;FOLD
(def-syntax fold
  (syntax-match (left right in)
     (sub (fold left (acc seed) (x in lst) (x* in lst*) ... new-acc)
           (: with-syntax (a a* ...) (generate-temporaries #'(x x* ...))
              #'(fold-left
                 (lambda (acc a a* ...)
                   (let+ ((x x* ...) (list a a* ...)) new-acc))
                 seed lst lst* ...)))
     (sub (fold right (acc seed) (x in lst) (x* in lst*) ... new-acc)
           (: with-syntax (a a* ...) (generate-temporaries #'(x x* ...))
              #'(fold-right
                 (lambda (a a* ... acc)
                   (let+ ((x x* ...) (list a a* ...)) new-acc))
                 seed lst lst* ...)))
     ))
;;END

;;FLATTEN
(define (flatten lst)
  (fold right (a '()) (x in lst)
        (if (list? x) (append (flatten x) a) (cons x a))))
;;END

;;PERM
;; compute the permutations of a list of distinct elements
(define (perm eq? lst)
  (cond
   ((null? lst) '()); empty list
   ((null? (cdr lst)) (list lst)); single element list
   (else; multi-element list 
    (list-of (cons el ls)
             (el in lst)
             (ls in (perm eq? (remp (cut eq? el <>) lst)))))))
;;END

;;NORMALIZE
(define (normalize ls)
  (map (syntax-match ()
         (sub n #'(n n) (identifier? #'n))
         (sub (n v) #'(n v) (identifier? #'n)))
       ls))
;;END
)
