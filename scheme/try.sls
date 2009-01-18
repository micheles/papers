(library (try)
(export try)
(import (rnrs) (sweet-macros))

; _try-except
(def-syntax _try-except
  (syntax-match (except)
     (sub (_try-except expr
           (except (e id id* ...) action ...)
           ...
           (except (err) else-action ...))
          #'(guard (err ((or (assertion-violation? err) (error? err))
                         (let ((who (condition-who err)))
                           (case who
                            ((id id* ...) (let ((e err)) action ...))
                            ...
                            (else else-action ...)
                            ))))
                 expr))
     (sub (_try-except expr
           (except (e id id* ...) action ...)
           ...)
          #'(_try-except expr
             (except (e id id* ...) action ...)
             ...
             (except (err) (raise err))))
     ))

; _try-finally
(def-syntax (_try-finally e e* ... (finally f f* ...))
  #'(dynamic-wind (lambda () #f) (lambda () e e* ...) (lambda () f f* ...)))

; try
(def-syntax try
  (syntax-match (except finally)
     (sub (try expr (finally f f* ...))
          #'(_try-finally expr (finally f f* ...)))
     (sub (try expr (except (e id ...) action ...) ...)
          #'(_try-except expr (except (e id ...) action ...) ...))
     (sub (try expr (except (e id ...) action ...) ... (finally f f* ...))
          #'(_try-finally
             (_try-except expr (except (e id ...) action ...) ... )
             (finally f f* ...)))
     ))
)
