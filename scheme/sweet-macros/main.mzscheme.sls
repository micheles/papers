#!r6rs
(library (sweet-macros)
(export locally syntax-match def-syntax syntax-expand)
(import (rnrs) (for (sweet-macros helper3) run expand))

(def-syntax (syntax-expand (macro . args))
  #'(syntax->datum ((macro <transformer>) #'(... (... (macro . args))))))
)
