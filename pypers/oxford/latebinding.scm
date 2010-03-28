(define (toplevel)
  (define a 1)
  (define (f)
    (display a))
  (set! a 2)
  (f))

(toplevel)
