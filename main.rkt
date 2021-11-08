#lang racketscript/base

(require racketscript/interop)

(define COUNT 100)

;; get ref to inner html of element with provided id
(define (get-elem id)
  ($ (#js*.document.getElementById id) 'innerHTML))

;; display count to element on dom!
(define (display-count tm)
  ($/:= (get-elem "clock") "asdf"))


(define (interval-fn)
  (displayln COUNT)
  (display-count (number->string COUNT))
  (set! COUNT (- COUNT 1)))

(define interval
  (($ 'setInterval) interval-fn 1000))

interval
