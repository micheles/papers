#!r6rs
(library (aps lang)
(export literal-replace : empty-ctxt unbound? symbol-identifier=? inject
        identifier-append identifier-prepend ct-eval get-name-from-define let1)
(import (rnrs) (sweet-macros))

(def-syntax (inject ctxt (pat val) ... templ)
  #'(with-syntax ((pat (datum->syntax ctxt val)) ...) templ))

;;LET1
(def-syntax let1
  (syntax-match ()
    (sub (let1 () lst e e* ...)
         #'(if (null? lst) (begin e e* ...)
               (apply error 'let1 "Too many elements" lst)))
    (sub (let1 (arg1 arg2 ... . rest) lst e e* ...)
         #'(let ((ls lst))
             (if (null? ls)
                 (apply error 'let1 "Missing arguments" '(arg1 arg2 ...))
                 (let1 arg1 (car ls)
                   (let1 (arg2 ... . rest) (cdr ls) e e* ...)))))
    (sub (let1 name value e e* ...)
         #'(letrec ((name value)) e e* ...)
         (identifier? #'name)
         (syntax-violation 'let1 "Argument is not an identifier" #'name))
    ))
;;END

;;SYMBOL-IDENTIFIER=?
(define (symbol-identifier=? id1 id2)
  (symbol=? (syntax->datum id1) (syntax->datum id2)))
;;END

;;UNBOUND?
(define empty-ctxt
  (generate-temporaries '(empty-ctxt)))

(define (unbound? id)
  (define stripped-id (datum->syntax #'empty-ctxt (syntax->datum id)))
  (free-identifier=? id  stripped-id))
;;END

;;LITERAL-REPLACE
(def-syntax literal-replace
  (syntax-match ()
    (sub (literal-replace name expr)
         #'(def-syntax name (identifier-syntax expr))
         (identifier? #'name))
    (sub (literal-replace (name . args) body body* ...)
         #'(literal-replace name (lambda args body body* ...)))
    ))
;;END

;;COLON
(def-syntax : ; colon macro
  (syntax-match ()
    (sub (: let-form e); do nothing
         #'e)
    (sub (: let-form e1 e2)
         (syntax-violation ': "Odd number of arguments" #'let-form))
    (sub (: let-form patt value rest ... expr)
         #'(let-form ((patt value)) (: let-form rest ... expr))
         (identifier? #'let-form)
         (syntax-violation ': "Not an identifier" #'let-form))
    ))
;;END

;;CT-EVAL;; probably useless having inject
(define (ct-eval form ctxt)
  (lambda (x) (datum->syntax ctxt (syntax->datum form))))
;;END

;;GET-NAME-FROM-DEFINE
(define get-name-from-define
  (syntax-match (define)
    (sub (define (name . args) body body* ...) #'name
         (identifier? #'name)
         (syntax-violation 'get-name-from-define "not a name" #'name))
    (sub (define name value) #'name
         (identifier? #'name)
         (syntax-violation 'get-name-from-define "not a name" #'name))
    ))
  
;;END

;;IDENTIFIER-APPEND
;; take an identifier and return a new one with an appended suffix
(define (identifier-append id . strings)
  (define id-str (symbol->string (syntax->datum id)))
  (datum->syntax id (string->symbol (apply string-append id-str strings))))
;;END

;;IDENTIFIER-PREPEND
;; take an identifier and return a new one with an prepended suffix
(define (identifier-prepend id . strings)
  (define prefix (apply string-append strings))
  (datum->syntax id (string->symbol
                     (string-append
                      prefix (symbol->string (syntax->datum id))))))
;;END

)
