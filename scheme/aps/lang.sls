#!r6rs
(library (aps lang)
(export :)
(import (rnrs) (sweet-macros))

;;COLON
(def-syntax :
  (syntax-match ()
    (sub (: let-form e)
         #'e)
    (sub (: let-form e1 e2)
         (syntax-violation ': "Odd number of arguments" #'(let-form e1 e2)))
    (sub (: let-form patt value rest ... expr)
         #'(let-form ((patt value)) (: let-form rest ... expr))
         (identifier? #'let-form)
         (syntax-violation ': "Not an identifier" #'let-form))
    ))
;;END
)
