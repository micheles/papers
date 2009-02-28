#!r6rs
(library (aps compat)
(export (rename (mzscheme:printf printf)
                (mzscheme:format format)
                (mzscheme:gensym gensym)
                (mzscheme:pretty-print pretty-print)))
(import (rnrs) (prefix (scheme) mzscheme:)))
