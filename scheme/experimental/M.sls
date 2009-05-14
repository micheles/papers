#!r6rs
(library (experimental M)
(export a)
(import (rnrs) (experimental L))
(when #f (m)); this line is never executed at runtime
(display "M instantiated\n")
)
