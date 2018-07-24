(library (table)
(export tbl table table-extend table-get)
(import (rnrs) (sweet-macros) (only (ikarus) gensym))

(def-syntax (tbl (name value) ...)
  #'(letrec ((name value) ...)
      (table (list (list 'name name) ...))))

(def-syntax (table-get t key else0 else1 ...)
  #'(let ((res (t key)))
      (if (missing-key? res)
          (begin else0 else1 ...)
          res)))

(define MISSING-KEY (gensym 'missing-key))

(define (missing-key? x)
  (eq? x MISSING-KEY))

(define append-unique append)

(define (table alist)
  (define full-list (append-unique (list (list '->alist alist)) alist))
  (lambda (k)
    (define ls (assq k full-list))
    (if ls (cadr ls) MISSING-KEY)))

(define (table-extend base-table ext)
  (table (append (base-table '->alist) ext)))

)
