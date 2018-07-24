#!r6rs
(library (experimental define+)
(export define-registered define+)
(import (rnrs) (for (experimental registry) expand))

;; save the content of the expand-time registry into a runtime definition
(define-syntax define-registered
  (lambda (x)
    (syntax-case x ()
        ((define-registered names)
         #`(define names '#,(registry))))))

(define-syntax define+
  (lambda (x)
    (syntax-case x ()
      ((define+ name value)
       #'(begin
           ;; dirty trick to get an expand time side effect
           (define-syntax dummy (begin (register #'name) (lambda (x) #f)))
           ;; the real definition
           (define name value))))))

)
