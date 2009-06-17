#!r6rs
(library (aps easy-test)
(export catch-error test run-tests runner run)
(import (rnrs) (aps compat) (sweet-macros))

;; helper macro
(def-syntax (catch-error body body* ...)
  #'(let*
        ((error-message #f)
         (result
          (guard (err ;; as a side effect, set! the error message if any
                  ((or (assertion-violation? err) (error? err)
                       (undefined-violation? err))
                   (set! error-message (condition-message err))))
                 body body* ...)))
      (if error-message error-message
          (error 'catch-error "Expected error, got none!"
                 '(body body* ...) '=> result))))
 
;; test macro
(def-syntax (test description expr expected)
  #'(lambda (cmd)
      (case cmd
        ((descr) description)
        ((values) (list expected expr 'expr))
        ((run) (equal? expr expected))
        (else (error 'test "Invalid command" cmd)))))

;; four helper functions
(define (print-nothing descr expected evalued-expr expr)
  (display ""))

(define (print-dot descr expected evalued-expr expr)
  (display "."))

(define (print-msg descr expected evalued-expr expr)
  (printf "\n~s failed\nExpected ~s, got ~s\nExpression was ~a\n"
          descr expected evalued-expr expr))

(define (print-stats successes failures)
  (define total (+ successes failures))
  (printf "\nRun ~a tests. ~a passed, ~a failed\n" total successes failures))
 
;; full runner
(define (run-tests print-success print-failure . tests)
  (let loop ((tests tests) (success 0) (failure 0))
    (if (null? tests)
        (list success failure)
        (let* ((test1 (car tests))
               (descr (test1 'descr)) (vals (test1 'values)))
          (if (test1 'run)
              (begin; the test succeeded
                (apply print-success descr vals)
                (loop (cdr tests) (+ 1 success) failure))
              (begin; the test failed
                (apply print-failure descr vals)
                (loop (cdr tests) success (+ 1 failure))))))))

;; runner factory
(define (runner print-success print-failure print-stats)
  (lambda tests
    (define succ-fail (apply run-tests print-success print-failure tests))
    (apply print-stats succ-fail)))

;; default runner
(define run (runner print-dot print-msg print-stats))

)
