#lang racketscript/base

(require
 racketscript/browser ; TODO use these bindings! How?
 racketscript/interop)

(define MINS-PARAM "mins")

;; query-param: String -> String
;; Get the URL query parameter assoc with the provided name
(define (query-param param-name)
  (define query-string #js.window.location.search)
  (define urlparams ($/new (#js*.URLSearchParams #js.query-string)))
  (#js.urlparams.get param-name))

;; Convert a number of minutes (potentially a string) to seconds
(define (get-time-in-seconds tm)
  (* ($/str tm) 60))

;; set-elem!: String String -> Void
;; Set the DOM element whose id is specified by V's content to val
(define (set-elem! id val)
  (define el (#js.document.getElementById ($/str id)))
  ($/:= #js.el.innerHTML ($/str val)))

;; now: -> Date
;; Get the current date
(define (now)
  ($/new #js*.Date))

;; goal-time: Natural -> Date
;; Get the time at which the timer should stop
(define (goal-time num-secs)
  (define cur-time (now))
  (#js.cur-time.setSeconds
   (+ (#js.cur-time.getSeconds) num-secs))
  cur-time)

;; mins-secs-diff: Time Time -> [Values Natural Natural]
;; get the difference between two times in minutes and seconds
(define (mins-secs-diff goal cur-time)
  (define dist (- goal cur-time))
  (define minutes (floor (/ dist 60000)))
  (define seconds (floor (/ dist 100)))

  ;; TODO: looks like modulo operator doesn't work so well!
  ;; (define minutes (/ (modulo dist (* 1000 60 60)) (* 1000 60)))
  ;; (define seconds (/ (modulo dist (* 1000 60)) 1000))
  (values minutes seconds))


(define NUM-SECS (get-time-in-seconds
               (query-param MINS-PARAM)))
(define INTERVAL 1000)
(define GOAL-TIME (goal-time NUM-SECS))

(define (interval-fn)
  (define cur-time (now))
  (println cur-time)
  (if (= 0 (- GOAL-TIME cur-time))
      (begin
        (#js*.clearInterval interval)
        (set-elem! "clock" "DONE!"))
      (let-values
          ([(mins secs) (mins-secs-diff GOAL-TIME cur-time)])
        (println mins)
        (println secs)
        (set-elem! "minutes" mins)
        (set-elem! "seconds" secs))))

(define interval
  (#js*.setInterval interval-fn INTERVAL))

interval
