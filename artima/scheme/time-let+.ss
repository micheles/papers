(import (rnrs) (ikarus) (aps list-utils) (repeat-macro))

(time (repeat 10000000 (let-values (((x y z) (values 1 2 3))) 'dummy)))
(time (repeat 10000000 (let+ (x y z) (list 1 2 3) 'dummy)))
