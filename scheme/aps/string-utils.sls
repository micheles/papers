(library (aps string-utils)
(export string-split)
(import (rnrs) (aps compat))

;;STRING-SPLIT
;; adapted from http://schemecookbook.org/Cookbook/StringSplit
;; to match Python behaviour
(define (string-split str ch)
  (define len (string-length str))
  (define (split a b)
    ;(printf "~a: ~a ~a\n" str a b)
    (cond
     ((>= b len)
      (if (= a b) '("")
          (list (substring str a b))))
     ((char=? ch (string-ref str b))
      (if (= a b) (let ((res (split (+ 1 a) (+ 1 b))))
                    (if (= 0 a) (cons "" res) res))
          (cons (substring str a b) (split b b))))
     (else (split a (+ 1 b)))))
  (split 0 0))
;;END
)
