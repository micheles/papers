#!r6rs
(library (experimental static-map)
(export static-map)
(import (rnrs) (for (rnrs) (meta -1)) (sweet-macros))

(def-syntax (static-map (name value) ...)
  #'(syntax-match (<names> name ...)
      (sub (ctx <names>) #''(name ...))
      (sub (ctx name) #'value)
      ...))
)
