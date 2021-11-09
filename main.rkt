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

(define (get-elem-by-id id)
  (#js.document.getElementById ($/str id)))

(define (set-html! elem body)
  ($/:= #js.elem.innerHTML body))

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
  (define minutes (floor (/ dist 60000))) ;; TODO nicer math
  (define seconds (floor (/ (- dist (* 60000 minutes)) 1000)))
  (values minutes seconds))

;; get the seconds left
(define (seconds-left goal cur-time)
  (define-values (mins secs) (mins-secs-diff goal cur-time))
  (+ (* mins 60) secs))

#; { Date Date -> Bool }
;; is there no significant difference between the two times?
(define (no-diff? goal curr-time)
  (define-values (mins secs) (mins-secs-diff goal curr-time))
  (and (= mins 0) (= secs 0)))

;; escape javascript semantics madness
(define (sanitize-number n)
  (string->number (js-string->string n)))

;; Get the seconds configured or provided for the timer
(define (get-seconds)
  (define mb-param (get-query-param MINS-PARAM))
  (define ms-param (get-query-param SECS-PARAM))
  (cond
    [(and (not (void? mb-param))
          (not (void? ms-param)))
     ;; TODO this was exhibiting JS string concat bug
     (+ (minutes->seconds (sanitize-number mb-param)) (sanitize-number ms-param))]
    [(not (void? mb-param)) (minutes->seconds
                             (sanitize-number mb-param))]
    [(not (void? ms-param))
     (sanitize-number ms-param)]
    [else DEFAULT-SECS]))


(define START-TIME (get-seconds))
(define TIME-LEFT START-TIME)
(define TIMER #f)

;; set the timer and the website state
(define (set-timer! goal-time cur-time)
  (define-values (mins secs) (mins-secs-diff goal-time cur-time))
  (set! TIME-LEFT (seconds-left goal-time cur-time))
  (set-elem! "minutes" mins)
  (set-elem! "seconds" secs))

;; Get ref to start button
(define start-button
  (get-elem-by-id "start-button"))

(define (pause-timer! in)
  (#js*.clearInterval in)
  ($/:= #js.start-button.innerHTML "start")
  (set! TIMER #f))

;; set initial timer
(define (set-initial-timer!)
  (set-timer!
   (get-stop-time START-TIME) (now)))

#; { Natural [Natural] -> Interval }
;; start a timer for a given amount of time and set interval
(define (start-timer tm [timeout 100])
  ($/:= #js.start-button.innerHTML "pause")
  (define goal-time (get-stop-time (+ 1 tm))) ;; Add 1 to show the first second for a whole second

  (define (interval-fn)
    (define cur-time (now))
    (set-timer! goal-time cur-time)
    (when (no-diff? goal-time cur-time)
      (pause-timer! interval)))

  (define interval
    (#js*.setInterval interval-fn timeout))
  (set! TIMER interval)
  interval)

;; Set the timer initially
(set-initial-timer!)

;; Toggle the timer
(define (toggle-timer)
  (cond
    ;; If the time left is 0, we're restarting the timer
    [(and (not TIMER) (= TIME-LEFT 0))
     (start-timer START-TIME)]
    ;; If we don't have a timer otherwise, we're resuming the timer
    [(not TIMER) (start-timer TIME-LEFT)]
    ;; If we have a timer, pause it (kill it)
    [TIMER (pause-timer! TIMER)]
    ;; Otherwise, start the timer?
    [else (start-timer TIME-LEFT)]))

(define (reset-timer!)
  (when TIMER (pause-timer! TIMER))
  (set-initial-timer!))


($/:= #js.start-button.onclick (λ (_) (toggle-timer)))

(define reset-button
  (get-elem-by-id "reset-button"))

($/:= #js.reset-button.onclick
      (λ (_) (reset-timer!)))


;; TODO
;; - set timer with ui
;; - add keyboard shortcuts
;; - don't make `minutes`, `seconds` labels shift in UI
