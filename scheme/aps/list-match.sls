(library (aps list-match)
(export list-match _match)
(import (rnrs) (sweet-macros) (aps lang) (aps cut))

;; see http://groups.google.com/group/comp.lang.scheme/msg/7701b9231835635f?hl=en

(def-syntax (list-match lst (sub pattern template guard ...) ...)
  #'(let ((obj lst))
      (cond
       ((_match obj pattern (list template) guard ...) => car) ...
       (else (error 'list-match "pattern failure" obj))))
  (for-all (cut raw-id=? 'sub <>) #'(sub ...)))

(def-syntax _match
  (syntax-match (quote quasiquote)
      (sub (_match obj pattern template)
       #'(_match obj pattern template #t))
      (sub (_match obj () template guard?)
       #'(and (null? obj) guard? template))
      (sub (_match obj underscore template guard?)
        #'(and guard? template)
        (raw-id=? #'_ #'underscore))
      (sub (_match obj var template guard?)
        #'(let ((var obj)) (and guard? template))
        (identifier? #'var))
      (sub (_match obj 'datum template guard?)
        #'(and (equal? obj 'datum) template))
      (sub (_match obj `datum template guard? )
        #'(and (equal? obj `datum) guard? template))
      (sub (_match obj (kar . kdr) template guard?)
        #'(let ((ob obj))
            (and (pair? ob)
               (let ((kar-obj (car ob)) (kdr-obj (cdr ob)))
                 (_match kar-obj kar (_match kdr-obj kdr template guard?))))))
      (sub (_match obj const template guard?)
        #'(and (equal? obj const) guard? template))))
)
