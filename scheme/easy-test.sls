(library (easy-test)
(export test print-nothing print-dot print-msg)
(import (rnrs) (only (ikarus) printf) (sweet-macros))

(define (print-nothing descr expr expected)
  (display ""))

(define (print-dot descr expr expected)
  (display "."))

(define (print-msg descr expr expected)
  (printf "\n'~a' failed. Expected ~a, got ~a\n" descr expected expr))
  
(def-syntax test
  (syntax-match (success failure =>)
    (=> (test (success print-success) (failure print-failure)
           (descr e1 e2 ... => expect) ...)
     #'(let ((n-success 0) (n-failure 0))
        (let ((expr (begin e1 e2 ...)) (expected expect))
         (if (equal? expr expected)
             (begin
               (set! n-success (+ 1 n-success))
               (print-success descr expr expected))
             (begin
               (set! n-failure (+ 1 n-failure))
               (print-failure descr expr expected))
             ))
       ...
       (list n-success n-failure)))
    (=> (test (descr e1 e2 ... => expect) ...)
     #'(test (success print-dot) (failure print-msg)
           (descr e1 e2 ... => expect) ...))
  ))
)
