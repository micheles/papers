#!r6rs
(library (aps record-syntax)
(export record-syntax)
(import (rnrs) (for (rnrs) (meta -1))
        (sweet-macros) (for (only (aps list-utils) range) expand run))

(def-syntax (record-syntax field-name ...)
  (with-syntax (((i ...) (range (length #'(field-name ...)))))
    #'(syntax-match (field-name ...)
        (sub (ctx v field-name)
             #'(vector-ref v i)) ...)))
)
