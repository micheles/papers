 (import (rnrs) (only (ikarus) pretty-print))

 ;; a very low-level approach
 (define (convert-for-into-loop begin-list)
   (assert (eq? 'begin (car begin-list)))
   `(begin
      ,@(map (lambda (expr)
               (if (eq? 'for (car expr)) (apply convert-for (cdr expr)) expr))
             (cdr begin-list))))

 ; i1 is subject to multiple evaluations, but let's ignore the issue
 (define (convert-for i from i0 to i1 . actions)
   ;; from must be 'from and to must be 'to
   (assert (and (eq? 'from from) (eq? 'to to)))
   `(let loop ((i ,i0))
      (unless (>= i ,i1) ,@actions (loop (+ i 1)))))

 (pretty-print
  (convert-for-into-loop
   '(begin
      (define n 3)
      (display "begin program\n")
      (for i from 1 to n (display i))
      (display "\nend program\n"))))
