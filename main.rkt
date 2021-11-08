#lang racketscript/base

(require racketscript/interop)

(define COUNT 100)

(define (intervalFn)
    (displayln COUNT)
    (set! COUNT (- COUNT 1)))

(define interval
  (($ 'setInterval) intervalFn 1000))

interval
