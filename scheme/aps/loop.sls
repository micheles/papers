(library (aps loop)
(export let/cc loop for)
(import (rnrs) (sweet-macros))

(def-syntax (let/cc name body body* ...)
  #'(call-with-current-continuation (lambda (name) body body* ...)))

(def-syntax loop
  (syntax-match (start end do next)
    (sub (loop (start (i i0) ...) (end cond? result)
               expr (next incr-i ...))
         #`(let/cc #,(datum->syntax #'loop 'break)
             (let loop* ((i i0) ...)
               (let/cc #,(datum->syntax #'loop 'continue)
                  (cond (cond? result) (else expr (loop* incr-i ...))))))
         (= (length #'(i ...)) (length #'(incr-i ...)))
         (syntax-violation 'loop "Mismatch loop variables/next variables"
                           #'((i ...) (incr-i ...))))
    (sub (loop (start (i i0) ...) expr (next incr-i ...))
         #'(loop (start (i i0) ...) (end #f (void))
                 expr (next incr-i ...)))
    (sub (loop expr)
         #'(loop (start) expr (next)))))

(def-syntax (for (i ...) (i0 ...) body body* ...)
  (let ((len1 (length #'(i ...))) (len2 (length #'(i0 ...))))
  (if (not (= len1 len2))
      (syntax-violation 'loop "Mismatch loop variables/init values"
                        #'((i ...) (i0 ...)) (list len1 len2))
  #`(let/cc #,(datum->syntax #'for 'break)
      (let #,(datum->syntax #'for 'loop) ((i i0) ...)
           body body* ...)))))

;; ; include yield
;; (def-syntax (generator body body* ...)
;;   (let yield
;;       body body* ...))

;; (define (generator-zip . generators)
;;   (generator ;; while not stop-iteration
;;    (yield (list-of (g) (g in generators)))))

;; (define (for action . generators)
;;   (define generator (apply generator-zip generators))
;;   (loop (from (value (generator))) (eqn? value stop-iteration)
;;    (do (apply action value)) ((generator))))
)
