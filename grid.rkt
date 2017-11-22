#lang racket

(require math)

(define (generate-row width)
  (sample (discrete-dist '(#t #f)) width))

(define (generate-grid width height)
  (for/fold ([grid '()])
            ([i height])
    (append grid (list (generate-row width)))))

(generate-grid 20 20)
