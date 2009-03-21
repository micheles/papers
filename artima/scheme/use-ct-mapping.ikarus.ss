(import (rnrs) (sweet-macros) (experimental ct-mapping))

(def-syntax color (ct-mapping (red #\R) (green #\G) (yellow #\Y)))

(display "Available colors: ")
(display (color <names>))
(display (list (color red) (color green) (color yellow)))
(newline)
