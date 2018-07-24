#!r6rs
(library (experimental dummy-ctxt)
 (export dummy-ctxt)
 (import (only (rnrs) define syntax))
 (define dummy-ctxt #'here)
 )
