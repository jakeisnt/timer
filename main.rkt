#lang racketscript/base

(require
 racketscript/browser
 racketscript/interop)

(define MINS-PARAM "mins")
(define SECS-PARAM "secs")
(define DEFAULT-SECS 300)

;; { String -> String }
;; Get the URL query parameter assoc with the provided name
(define (get-query-param param-name)
  (define query-string #js.window.location.search)
  (define urlparams ($/new (#js*.URLSearchParams #js.query-string)))
  (#js.urlparams.get param-name))

#; { String -> Natural }
;; Convert a number of minutes (potentially a string) to seconds
(define (minutes->seconds tm)
  (* ($/str tm) 60))

#; { String String -> Void }
;; Set the DOM element whose id is specified by V's content to val
(define (set-elem! id val)
  (define el (#js.document.getElementById ($/str id)))
  ($/:= #js.el.innerHTML ($/str val)))

#; { -> Date }
;; Get the current date
(define (now)
  ($/new #js*.Date))

#; { Natural -> Date }
;; Get the time at which the timer should stop
(define (get-stop-time num-secs)
  (define cur-time (now))
  (#js.cur-time.setSeconds
   (+ (#js.cur-time.getSeconds) num-secs))
  cur-time)

#; { Time Time -> (Values Natural Natural) }
;; get the difference between two times in minutes and seconds
(define (mins-secs-diff goal cur-time)
  (define dist (- goal cur-time))
  (define minutes (floor (/ dist 60000)))
  (define seconds (floor (/ (- dist (* 60000 minutes)) 1000)))
  (values minutes seconds))

#; { Date Date -> Bool }
;; is there no significant difference between the two times?
(define (no-diff? goal curr-time)
  (define-values (mins secs) (mins-secs-diff goal curr-time))
  (and (= mins 0) (= secs 0)))

;; Get the seconds configured or provided for the timer
(define (get-seconds)
  (define mb-param (get-query-param MINS-PARAM))
  (define ms-param (get-query-param MINS-PARAM))
  (cond
    [(not (void? mb-param)) (minutes->seconds mb-param)]
    [(not (void? ms-param)) ms-param]
    [else DEFAULT-SECS]))


#; { Natural [Natural] -> Interval }
;; start a timer for a given amount of time and set interval
(define (start-timer tm [timeout 500])
  (define goal-time (get-stop-time tm))
  (define (interval-fn)
    (define cur-time (now))
    (define-values (mins secs) (mins-secs-diff goal-time cur-time))
    (if (no-diff? goal-time cur-time)
        (begin
          (#js*.clearInterval interval)
          (set-elem! "clock" "DONE!"))
        (begin
          (set-elem! "minutes" mins)
          (set-elem! "seconds" secs))))
  (define interval
    (#js*.setInterval interval-fn timeout))
  interval)


;; (println (minutes->seconds (get-query-param MINS-PARAM)))
(println (get-seconds))

(start-timer (get-seconds))

;; TODO
;; - pause and play timer
;; - don't start timer on page load
;; - set timer with ui as well as through url
;; - enable mins:secs query parameter for time configuration
;; - improve UI!
