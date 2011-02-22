#!r6rs
(library (aps compat)
(export printf format gensym pretty-print)
(import (rnrs) (only (scheme) printf format gensym pretty-print)))
