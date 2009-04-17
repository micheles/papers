(import (rnrs) (sweet-macros) (only (aps list-utils) distinct?))

(def-syntax (assert-distinct arg ...)
  #'(#f)
  (distinct? free-identifier=? #'(arg ...))
  (syntax-violation 'assert-distinct "Duplicate name" #'(arg ...)))

