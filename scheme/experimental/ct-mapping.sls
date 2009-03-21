#!r6rs
(library (experimental ct-mapping)
(export ct-mapping)
(import (rnrs) (sweet-macros))

(def-syntax (ct-mapping (name value) ...)
  #'(syntax-match (<names> name ...)
      (sub (ctx <names>) #''(name ...))
      (sub (ctx name) #'value)
      ...))
)
