#!r6rs

(library (sweet-macros)
(export def-syntax syntax-match syntax-expand)
(import (rnrs) (for (sweet-macros helper2) run expand))

(def-syntax (syntax-expand (macro . args))
  #'(syntax->datum ((macro <transformer>) #'(macro . args))))
)
