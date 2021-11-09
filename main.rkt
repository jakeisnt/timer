#lang racketscript/base

(require
 racketscript/browser ; TODO use these bindings!
 racketscript/interop)

(define COUNT 100)

;; get ref to inner html of element with provided id
;; (define (get-elem id)
;;   ($ (#js*.document.getElementById id) 'innerHTML))

(define (set-elem! id val)
  (define el (#js.document.getElementById ($/str id)))
  (println el)
  ($/:= #js.el.innerHTML ($/str val)))


(define (interval-fn)
  (displayln COUNT)
  (set-elem! "root" COUNT)
  (set! COUNT (- COUNT 1)))

(define interval
  (($ 'setInterval) interval-fn 1000))

(set-elem! "root" "test!!!!!!!!!!!!")

interval
