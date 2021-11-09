#lang racketscript/base

(require
 racketscript/browser
 racketscript/interop)

(define MINS-PARAM "mins")
(define SECS-PARAM "secs")
(define DEFAULT-SECS 300)

(define (screen-width)
  (or #js.document.documentElement.clientWidth
      #js.document.body.clientWidth
      #js.window.innerWidth))

(define (screen-height)
  (or #js.document.documentElement.clientHeight
      #js.document.body.clientHeight
      #js.window.innerHeight))

(define (cur-performance)
  (#js*.performance.now))

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

(define (create-elem tagname)
  (#js.document.createElement ($/str tagname)))

;; add image to DOM, returning its ref, width and height
(define (add-img path)
  (define img (create-elem "img"))
  ($/:= #js.img.src path)
  img)

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
  (if (= mins 1)
    (set-elem! "mins-label" "minute")
    (set-elem! "mins-label" "minutes"))
  (if (= secs 1)
    (set-elem! "secs-label" "second")
    (set-elem! "secs-label" "seconds"))
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

(define (on-game-end! in)
  (pause-timer! in)
  (on-game-end "lerner.png" -100)
  (on-game-end "amal.png" 0 -100 0.2 0.5))


#; { Natural [Natural] -> Interval }
;; start a timer for a given amount of time and set interval
(define (start-timer tm [timeout 100])
  ($/:= #js.start-button.innerHTML "pause")
  (define goal-time (get-stop-time (+ 1 tm))) ;; Add 1 to show the first second for a whole second

  (define (interval-fn)
    (define cur-time (now))
    (set-timer! goal-time cur-time)
    (when (no-diff? goal-time cur-time)
      (on-game-end! interval)))

  (define interval
    (#js*.setInterval interval-fn timeout))
  (set! TIMER interval)
  interval)

;; Set the timer initially
(set-initial-timer!)

;; Toggle the timer
(define (toggle-timer!)
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


($/:= #js.start-button.onclick
      (λ (_) (toggle-timer!)))

(define reset-button
  (get-elem-by-id "reset-button"))

($/:= #js.reset-button.onclick
      (λ (_) (reset-timer!)))


;; Keyboard shortcuts
;; TODO make cool macro for this
($/:= #js.document.onkeyup
      (λ (e)
        (when (= #js.e.keyCode 32)
          (toggle-timer!))))


(define (on-game-end pic-name
                     [start-x 0]
                     [start-y 0]
                     [vx-default 0.3]
                     [vy-default 0.3])
  (define img (add-img pic-name))

  ($/:= #js.img.style.position "absolute")
  ($/:= #js.img.style.zIndex "999")
  ($/:= #js.img.style.top "0")
  ($/:= #js.img.style.left "0")
  (#js.document.body.appendChild #js.img)

  (define x start-x)
  (define y start-y)

  ; TODO theses should be fetched dynamically,
  ; but the typical javascript  ways fetched 0?
  (define w 80)
  (define h 80)

  (define vx vx-default)
  (define vy vy-default)

  (define prev (cur-performance))

  (define (frame ts)
    (define W (screen-width))
    (define H (screen-height))

    (define dt (- ts prev))
    (set! x (max 0 (min (- W w) (+ x (* dt vx)))))
    (set! y (max 0 (min (- H h) (+ y (* dt vy)))))

    ($/:= #js.img.style.transform
          ;; TODO didn't work without number->string, silently failed
          (js-string (string-append "translate(" (number->string x) "px, " (number->string y) "px)")))

    (when (or (<= x 0) (>= (+ x w) W))
      (set! vx (* vx -1)))

    (when (or (<= y 0) (>= (+ y h) H))
      (set! vy (* vy -1)))

    (set! prev ts)
    (#js.window.requestAnimationFrame frame))
  (#js.window.requestAnimationFrame frame))


;; TODO
;; - set timer with ui
;; - animation and sounds when timer ends
