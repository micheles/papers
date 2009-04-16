#!r6rs
(library (aps lang)
(export define-ct : identifier-append identifier-prepend get-name-from-define)
(import (rnrs) (sweet-macros))

;;DEFINE-CT
(def-syntax define-ct
  (syntax-match ()
    (sub (define-ct name expr)
         #'(def-syntax name (identifier-syntax expr))
         (identifier? #'name))
    (sub (define-ct (name . args) body body* ...)
         #'(define-ct name (lambda args body body* ...)))
    ))
;;END

;;COLON
(def-syntax :
  (syntax-match ()
    (sub (: let-form e)
         #'e)
    (sub (: let-form e1 e2)
         (syntax-violation ': "Odd number of arguments"
          (syntax->datum #'(let-form e1 e2))))
    (sub (: let-form patt value rest ... expr)
         #'(let-form ((patt value)) (: let-form rest ... expr))
         (identifier? #'let-form)
         (syntax-violation ': "Not an identifier" #'let-form))
    ))
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
