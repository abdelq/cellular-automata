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

(define (next grid x y birth survival)
  (let ([alive? (list-ref (list-ref grid y) x)]
        [alive-neighbors (count identity (neighbors grid x y))])
    (cond
      [(and alive? (>= alive-neighbors survival)) #t]
      [(and (not alive?) (>= alive-neighbors birth)) #t]
      [else #f])))

(define (iterate grid birth survival)
  (let ([height (length grid)]
        [width (length (first grid))])
    (for/list ([y height])
      (for/list ([x width])
        (next grid x y birth survival)))))

(define (automata width height alive birth survival iterations)
  (let ([grid (generate width height alive)])
    (for ([i iterations])
      (set! grid (iterate grid birth survival)))
    grid))

(pretty-print (automata 20 20 0.45 5 4 4))
