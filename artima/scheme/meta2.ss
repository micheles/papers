#!r6rs
(import (rnrs)
        (for (sweet-macros) (meta 0) (meta 1))
        (for (only (rnrs) begin lambda display) (meta 2)))

(def-syntax m
  (let ()    
    (def-syntax m2
      (begin                                ;; begin, display and 
        (display "at metalevel 2\n")        ;; lambda are used here 
        (lambda (x) "expanded-m\n")))       ;; at meta-level 2
    (define _ (display "at metalevel 1\n")) ;; meta-level 1
    (lambda (x) (m2))))                     ;; here

(display (m))
