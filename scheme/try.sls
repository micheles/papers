(library (try)
(export try)
(import (rnrs) (sweet-macros))

;; _TRY-EXCEPT
(def-syntax _try-except
  (syntax-match (except)
     (sub (_try-except expr
           (except (e id id* ...) action ...)
           ...
           (except (err) else-action ...))
          #'(guard
             (err
              ((or (assertion-violation? err) (error? err))
               (case (condition-who err)
                 ((id id* ...) (let ((e err)) action ...))
                 ...
                 (else else-action ...)
                 )))
             expr))
     (sub (_try-except expr
           (except (e id id* ...) action ...)
           ...)
          #'(_try-except expr
             (except (e id id* ...) action ...)
             ...
             (except (err) (raise err))))
     ))
;; END

;; _TRY-FINALLY
(def-syntax (_try-finally e e* ... (finally f f* ...))
  #'(dynamic-wind
        (lambda () #f)
        (lambda () e e* ...)
        (lambda () f f* ...)))
;; END

;; TRY
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
;; END
)
