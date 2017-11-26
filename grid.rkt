#lang racket

(require math
         racket/draw)

(define (inside? grid x y)
  (let ([height (length grid)]
        [width (length (first grid))])
    (and (>= y 0)
         (< y height)
         (>= x 0)
         (< x width))))

(define (neighbors grid x y)
  (for*/list ([i (in-range (- x 1) (+ x 2))]
              [j (in-range (- y 1) (+ y 2))]
              #:unless (equal? (cons i j) (cons x y)))
    (if (inside? grid i j)
      (list-ref (list-ref grid j) i)
      #t)))

(define (iter grid x y birth survival)
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
        (iter grid x y birth survival)))))

(define (generate width height alive)
  (for/list ([i height])
    (sample (discrete-dist '(#t #f) (list alive (- 1 alive))) width)))

(define (automata width height alive birth survival iterations)
  (let ([grid (generate width height alive)])
    (for ([i iterations])
      (set! grid (iterate grid birth survival)))
    grid))

(let* ([dungeon (automata 100 100 .45 5 4 12)]
       [grid (make-bitmap (length (first dungeon)) (length dungeon))]
       [dc (new bitmap-dc% [bitmap grid])])
  (send dc set-pen "black" 0 'solid)
  (send dc set-brush "black" 'solid)
  (for* ([y (length dungeon)]
         [x (length (first dungeon))])
      (when (list-ref (list-ref dungeon y) x)
        (send dc draw-rectangle x y 1 1)))
  (send grid save-file "dungeon.png" 'png))
