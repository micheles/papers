(library (aps list-match)
(export list-match _match)
(import (rnrs) (sweet-macros))

;; see http://groups.google.com/group/comp.lang.scheme/msg/7701b9231835635f?hl=en

(def-syntax (list-match lst (sub pattern template guard ...) ...)
  #'(let ((obj lst))
      (cond
       ((_match obj pattern (list template) guard ...) => car) ...
       (else (error 'list-match "pattern failure" obj))))
  (for-all (lambda (s) (eq? (syntax->datum s) 'sub)) #'(sub ...)))

(def-syntax _match
  (syntax-match (quote quasiquote)
      (sub (_ obj pattern template)
       #'(_match obj pattern template #t))
      (sub (_ obj () template guard)
       #'(and (null? obj) guard template))
      (sub (_ obj underscore template guard)
        #'(and guard template)
        (and (identifier? #'underscore) (free-identifier=? #'underscore #'_)))
      (sub (_ obj var template guard)
        #'(let ((var obj)) (and guard template))
        (identifier? (syntax var)))
      (sub (_ obj (quote datum) template guard)
        #'(and (equal? obj (quote datum)) template))
      (sub (_ obj (quasiquote datum) template guard )
        #'(and (equal? obj (quasiquote datum)) guard template))
      (sub (_ obj (kar . kdr) template guard)
        #'(let ((ob obj))
            (and (pair? ob)
               (let ((kar-obj (car ob)) (kdr-obj (cdr ob)))
                 (_match kar-obj kar
                    (_match kdr-obj kdr template guard))))))
      (sub (_ obj const template guard)
        #'(and (equal? obj const) guard template))))
)
