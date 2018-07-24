#!r6rs
(library (aps string-utils)
(export string-split drop1 collect-symbols-starting-with
        replace-with-commas)
(import (rnrs) (aps compat) (aps list-utils))

(define (string-cdr s)
  (list->string (cdr (string->list s))))

(define (drop1 s);; drop the first character from a symbol
  (string->symbol (list->string (cdr (string->list (symbol->string s))))))

(define (collect-symbols-starting-with char lst)
  (remove-dupl symbol=?
   (deep-fold
    (lambda (x a)
      (if (and (symbol? x) (char=? char (string-ref (symbol->string x) 0)))
          (cons x a) a)) '() lst)))

(define (replace-with-commas char lst)
  (deep-map
   (lambda (x)
     (if (and (symbol? x) (char=? char (string-ref (symbol->string x) 0)))
         (list 'unquote x) x)) lst))

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
