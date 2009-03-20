#!r6rs
(library (sweet-macros)
(export syntax-match def-syntax syntax-expand)
(import (rnrs) (for (sweet-macros helper3) run expand))

;; this only works for macros defined through def-syntax
(def-syntax (syntax-expand (macro . args))
  #'(syntax->datum ((macro <transformer>) #'(... (... (macro . args))))))
)
