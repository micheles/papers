(library (repeat-macro)
(export repeat)
(import (rnrs) (sweet-macros))

(def-syntax (repeat n body body* ...)
  #'(let loop ((i 0))
      (when (< i n) body body* ... (loop (+ 1 i)))))

)
