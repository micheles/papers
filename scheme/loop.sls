(import (rnrs) (sweet-macros))

(def-syntax (let/cc name body body* ...)
  #'(call-with-current-continuation (lambda (name) body body* ...)))

(def-syntax loop
  (syntax-match (start end do next)
    (sub (loop (start (i i0) ...) (end cond? result)
               (do body body* ...) (next incr-i ...))
         #`(let/cc #,(datum->syntax #'loop 'break)
             (let loop* ((i i0) ...)
               (let/cc #,(datum->syntax #'loop 'continue)
                  body body* ...
                  (if cond? result (loop* incr-i ...)))))
         (= (length #'(i ...)) (length #'(incr-i ...)))
         (syntax-violation 'loop "Mismatch loop variables/next variables"
                           #'((i ...) (incr-i ...))))
    (sub (loop (start (i i0) ...) (do body body* ...) (next incr-i ...))
         #'(loop (start (i i0) ...) (end #f (void))
                 (do body body* ...) (next incr-i ...)))
    (sub (loop (do body body* ...))
         #'(loop (start) (do body body* ...) (next)))))

; include yield
;(def-syntax (generator body body* ...)
;  (let yield
;      body body* ...))

(define (generator-zip . generators)
  (generator ;; while not stop-iteration
   (yield (list-of (g) (g in generators)))))

(define (for action . generators)
  (define generator (apply generator-zip generators))
  (loop (from (value (generator))) (eqn? value stop-iteration)
   (do (apply action value)) ((generator))))
