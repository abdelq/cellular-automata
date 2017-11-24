#lang racket

(require math)

(define (gen-grid width height alive)
  (for/list ([i height])
    (sample (discrete-dist '(#t #f) `(,alive ,(- 1 alive))) width)))

(pretty-print (gen-grid 10 10 .5))
