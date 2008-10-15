(library (repeat-macro)
(export repeat)
(import (rnrs) (sweet-macros))

(def-syntax (repeat n expr ...)
  #'(let loop ((i 0))
      (when (< i n) expr ... (loop (+ 1 i)))))

)
