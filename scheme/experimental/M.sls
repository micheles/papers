#!r6rs
(library (experimental M)
(export a)
(import (rnrs) (experimental L))
(m); this line is expanded at compile-time
(display "M instantiated\n"); at run-time
)
