;; run this with ikarus -d
(let loop ((x -3))
  (/ 1 x)
  (loop (add1 x)))
