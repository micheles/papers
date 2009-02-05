(library (aps test-utils)
;;; Version: 0.5
;;; Author: Michele Simionato
;;; Email: michele.simionato@gmail.com
;;; Date: 31-Jan-2009
;;; Licence: BSD
(export test run-tests runner run print-nothing print-dot print-msg)
(import (rnrs) (only (ikarus) printf) (sweet-macros))

;; test macro
(def-syntax (test description expr expected)
  #'(lambda (cmd)
      (case cmd
        ((descr) description)
        ((values) (list expected expr 'expr))
        ((run) (equal? expr expected))
        (else (error 'test "Invalid command" cmd)))))

;; three helper functions
(define (print-nothing descr expected evalued-expr expr)
  (display ""))

(define (print-dot descr expected expr evalued-expr)
  (display "."))

(define (print-msg descr expected expr evalued-expr)
  (printf "\n'~a' failed. Expected ~a, got ~a for ~a\n"
          descr expected expr evalued-expr))

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
(define (runner print-success print-failure)
  (lambda tests (apply run-tests print-success print-failure tests)))

;; default runner
(define run (runner print-dot print-msg))

)
