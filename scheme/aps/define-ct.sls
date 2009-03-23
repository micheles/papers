(library (aps define-ct)
(export alist define-ct)
(import (rnrs) (sweet-macros) (aps lang) (aps list-utils))

;;DEFINE-CT
(def-syntax (define-ct kw (define name value) ...)
  #'(def-syntax kw
      (let ((a (alist (name value) ...)))
        (syntax-match (name ...)
          (sub (kw name) (datum->syntax #'kw (car (assq 'name a))))
          ...)))
  (eq? (syntax->datum #'define) 'define))
;;END

)
