#lang racketscript

(require
 racketscript/htdp/image
 racketscript/htdp/universe
 racket/match
 racket/list)


(define (start)
  (big-bang 0
            (on-tick (lambda (a) (println a)))))

(start)
