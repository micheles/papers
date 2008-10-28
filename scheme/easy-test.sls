(library (easy-test)
(export test run-tests runner run print-nothing print-dot print-msg)
(import (rnrs) (only (ikarus) printf) (sweet-macros))

;; three helper functions
(define (print-nothing descr expr expected)
  (display ""))

(define (print-dot descr expr expected)
  (display "."))

(define (print-msg descr expr expected)
  (printf "\n'~a' failed. Expected ~a, got ~a\n" descr expected expr))

;; test macro
(def-syntax (test description expr expected)
  #'(lambda (cmd)
      (case cmd
        ((descr) description)
        ((values) '(expr  expected))
        ((run) (equal? expr expected))
        (else (error 'test "Invalid command" cmd)))))

;; full runner
(define (run-tests print-success print-failure . tests)
  (let loop ((tests tests) (success 0) (failure 0))
    (if (null? tests)
        (list success failure)
        (let* ((test1 (car tests))
               (descr (test1 'descr)) (vals (test1 'values)))
          (if (test1 'run); the test succeeded
              (begin
                (apply print-success descr vals)
                (loop (cdr tests) (+ 1 success) failure))
              (begin
                (apply print-failure descr vals)
                (loop (cdr tests) success (+ 1 failure))))))))

;; runner factory
(define (runner print-success print-failure)
  (lambda tests (apply run-tests print-success print-failure tests)))

;; default runner
(define run (runner print-dot print-msg))

)
