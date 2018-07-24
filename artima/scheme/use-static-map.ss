(import (rnrs) (sweet-macros) (for (experimental static-map) expand))
;; the for syntax is ignored in implementations with implicit phasing

(def-syntax color (static-map (red #\R) (green #\G) (yellow #\Y)))

(display "Available colors: ")
(display (color <names>))
(display (list (color red) (color green) (color yellow)))
(newline)
