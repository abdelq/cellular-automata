#lang racket

(require math)

(define (generate width height alive)
  (for/list ([i height])
    (sample (discrete-dist '(#t #f) (list alive (- 1 alive))) width)))

(define (inside? grid x y)
  (let ([height (length grid)]
        [width (length (first grid))])
    (and (>= x 0)
         (< x width)
         (>= y 0)
         (< y height))))

(define (neighbors grid x y)
  (for*/list ([i (in-range (- y 1) (+ y 2))]
              [j (in-range (- x 1) (+ x 2))]
              #:unless (equal? (cons i j) (cons y x)))
    (if (inside? grid j i)
      (list-ref (list-ref grid i) j)
      #t)))

(let ([grid (generate 10 10 .5)]
      [x 1]
      [y 1])
  (pretty-print grid)
  (neighbors grid x y))
