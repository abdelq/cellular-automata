#lang racket

(require math)

(define (gen-grid width height alive)
  (for/list ([i height])
    (sample (discrete-dist '(#t #f) `(,alive ,(- 1 alive))) width)))

(define (neighbors grid x y)
  (for*/list ([i (in-range (- y 1) (+ y 2))]
              [j (in-range (- x 1) (+ x 2))]
              #:unless (or (equal? (cons i j) (cons y x))
                           (< i 0) (>= i (length grid))
                           (< j 0) (>= j (length (first grid)))))
    (list-ref (list-ref grid i) j)))

(let ([grid (gen-grid 10 10 .5)]
      [x 5] [y 5])
  (pretty-print grid)
  (neighbors grid x y))
