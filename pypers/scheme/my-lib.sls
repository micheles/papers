 #!r6rs
 (library (my-lib)
           (export a b)
            (import (rnrs))
             (define a 42)
              (define b 0)
               (display "my-lib instantiated!\n")
 )
