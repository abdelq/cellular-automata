#lang racket

(require math)

(define (gen-grid width height)
  (for/list ([i height])
    (sample (discrete-dist '(#t #f)) width)))

(pretty-print (gen-grid 10 10))
