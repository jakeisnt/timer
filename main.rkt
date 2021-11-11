#lang racketscript/base

(require
 racketscript/browser
 racketscript/interop)

(define MINS-PARAM "mins")
(define SECS-PARAM "secs")
(define FLIP-PARAM "flip")
;; 1 second
(define MIN-SECS 1)
;; just under 1 hour
(define MAX-SECS (+ (* 59 60) 59))
(define DEFAULT-SECS 300)

;; ----- DOM -----

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

;; { String -> [Maybe String] }
;; Get the URL query parameter assoc with the provided name
(define (get-query-param param-name)
  (define query-string #js.window.location.search)
  (define urlparams ($/new (#js*.URLSearchParams #js.query-string)))
  (define param-result (#js.urlparams.get ($/str param-name)))
  (if (void? param-result) #f (js-string->string param-result)))

(define (get-elem-by-id id)
  (#js.document.getElementById ($/str id)))

(define (set-elem! elem body)
  ($/:= #js.elem.innerHTML body))

#; { String String -> Void }
;; Set the DOM element whose id is specified by V's content to val
(define (set-elem-by-id! id val)
  (define el (#js.document.getElementById ($/str id)))
  (set-elem! el ($/str val)))

;; Create an HTML element with the provided tag name
(define (create-elem tagname)
  (#js.document.createElement ($/str tagname)))

;; add image to DOM, returning its ref, width and height
(define (add-img path)
  (define img (create-elem "img"))
  ($/:= #js.img.src path)
  img)

#; { String -> Audio }
;; Load a sound from a file!
(define (load-sound path)
  (define sound ($/new (#js*.Audio #js.path)))
  sound)

#; { Audio -> Audio }
;; Play audio
;; Assume that the audio has loaded by the time this is invoked!
(define (play-sound sound)
  (#js.sound.play)
  sound)

#; { Audio -> Audio }
;; Pause sound that's playing
(define (pause-sound sound)
  (#js.sound.pause)
  sound)


;; ----- Utils -----

(define (pad-str s) s)

#; { [Maybe String] -> [Maybe Natural] }
;; Parse a number from a string if the string exists
;; If the string can't be a number, or the number is not a natural, return #f
(define (sanitize-number n)
  (and n (number? (string->number n))
      (let ([num (string->number n)])
        (if (< num 0) #f (floor num)))))

#; { Number Number -> [Number -> Number] }
;; Generate a function to ensure a number is between `a` and `b`
;; Assume that the number lies within
(define (ensure-in a b)
  (lambda (n)
    (cond
      [(< n a) a]
      [(> n b) b]
      [else n])))

(define ensure-in-secs
  (ensure-in MIN-SECS MAX-SECS))


;; ----- Time -----

#; { Natural -> Natural }
;; Convert a number of minutes (potentially a string) to seconds
(define (minutes->seconds tm)
  (* tm 60))

;; convert seconds to appropriate num mins and seconds
(define (seconds->mins+secs secs)
  (define minutes (floor (/ secs 60)))
  (define seconds (floor (- secs (* minutes 60))))
  (values minutes seconds))

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

;; diff in seconds between two dates
(define (seconds-left goal cur-time)
  (floor (/ (- goal cur-time) 1000)))

#; { Time Time -> (Values Natural Natural) }
;; get the difference between two times in minutes and seconds
(define (mins-secs-diff goal cur-time)
  (seconds->mins+secs (seconds-left goal cur-time)))

#; { Date Date -> Bool }
;; is there no significant difference between the two times?
(define (no-diff? goal curr-time)
  (= 0 (seconds-left goal curr-time)))

;; get the time in minutes and seconds between two times
;; if flip, flip the result corresponding to some max time
(define (get-time-between goal-time cur-time [flip #f] [max-time 0])
  (define secs-left (seconds-left goal-time cur-time))
  (if flip
    (seconds->mins+secs (- max-time secs-left))
    (seconds->mins+secs secs-left)))


;; ----- Start / Settings -----

#; { -> Maybe Natural }
;; Get the setting for number of seconds if the parameter is defined
(define (get-seconds-param)
  (sanitize-number (get-query-param SECS-PARAM)))


#; { -> Maybe Natural }
;; Get the setting for number of minutes if the parameter is defined
(define (get-minutes-param)
  (sanitize-number (get-query-param MINS-PARAM)))

#; { -> Natural }
;; Get the number of seconds configured for or provided for the timer
(define (get-seconds)
  (define mb-mins (get-minutes-param))
  (define mb-secs (get-seconds-param))

  (ensure-in-secs
   (cond
    [(and mb-mins mb-secs)
     (+ (minutes->seconds mb-mins) mb-secs)]
    [mb-mins (minutes->seconds mb-mins)]
    [mb-secs mb-secs]
    [else DEFAULT-SECS])))


#; { -> Bool }
;; should the timer start flipped?
(define (get-flip-param)
  (equal? "t" (get-query-param FLIP-PARAM)))


;; ----- Stateful Game -----

(define START-TIME (get-seconds))
(define TIME-LEFT START-TIME)
(define TIMER #f)
(define FLIP (get-flip-param))
(define ALARM (load-sound "assets/alarm.mp3"))


;; set the timer and the state of the site
(define (set-timer! goal-time cur-time)
  (define-values (mins secs)
        (get-time-between goal-time cur-time FLIP START-TIME))
  (set! TIME-LEFT (seconds-left goal-time cur-time))

  (if (= mins 1)
    (set-elem-by-id! "mins-label" "minute")
    (set-elem-by-id! "mins-label" "minutes"))
  (if (= secs 1)
    (set-elem-by-id! "secs-label" "second")
    (set-elem-by-id! "secs-label" "seconds"))
  (set-elem-by-id! "minutes" (pad-str mins))
  (set-elem-by-id! "seconds" (pad-str secs)))


;; ----- Event Handlers -----

#; { Natural [Natural] -> Interval }
;; start a timer for a given amount of time and set interval
(define (start-timer tm [timeout 100])
  ($/:= #js.start-button.innerHTML "pause")
  (define goal-time (get-stop-time (+ 1 tm))) ;; Add 1 to show the first second for a whole second

  (define (interval-fn)
    (define cur-time (now))
    (set-timer! goal-time cur-time)
    (when (no-diff? goal-time cur-time)
      (on-timer-end! interval)))

  (define interval
    (#js*.setInterval interval-fn timeout))
  (set! TIMER interval)
  interval)

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

;; reset the timer to the starting timer
(define (reset-timer!)
  (when TIMER (pause-timer! TIMER))
  (set-initial-timer!))

;; set up the game's initial timer values
(define (set-initial-timer!)
  (set-timer!
   (get-stop-time START-TIME) (now)))

;; pause the referenced timer, saving its state
(define (pause-timer! in)
  (#js*.clearInterval in)
  ($/:= #js.start-button.innerHTML "start")
  (set! TIMER #f))

;; flip the current time!
;; if we're counting up, count down, and vice versa
(define (flip!)
  (set! FLIP (not FLIP))
  (if FLIP
      ($/:= #js.flip-button.innerHTML "count down")
      ($/:= #js.flip-button.innerHTML "count up"))
  (set-timer!
   (get-stop-time TIME-LEFT) (now)))

;; show the timer end animation
(define (on-timer-end! in)
  (pause-timer! in)
  (play-sound ALARM)
  (define (end)
    (on-game-end
      (if (= 0 (random 2))
        "assets/lerner.png"
        "assets/amal.png")
      (random (screen-width))
      (random (screen-height))
      (- (random) 0.5)
      (- (random) 0.5)))
  (#js*.setInterval end 250))


;; ----- Event Binding -----
(define start-button
  (get-elem-by-id "start-button"))

($/:= #js.start-button.onclick
      (λ (_) (toggle-timer!)))

(define reset-button
  (get-elem-by-id "reset-button"))

($/:= #js.reset-button.onclick
      (λ (_) (reset-timer!)))

(define flip-button
  (get-elem-by-id "flip-button"))

($/:= #js.flip-button.onclick
      (λ (_) (flip!)))

;; Keyboard shortcuts
;; TODO make cool macro for this
($/:= #js.document.onkeyup
      (λ (e)
        (when (= #js.e.keyCode 32)
          (toggle-timer!))))


;; Set the timer initially
(set-initial-timer!)
;; flip twice to get original settings
(flip!)
(flip!)


;; ----- Scary Stuff -----

;; invoke animation
(define (on-game-end pic-name
                     [start-x 0]
                     [start-y 0]
                     [vx-default 0.3]
                     [vy-default 0.3])
  (define img (add-img pic-name))

  ;; TODO improve ergonomics of setting styles dynamically
  ($/:= #js.img.style.position "absolute")
  ($/:= #js.img.style.zIndex "999")
  ($/:= #js.img.style.top "0")
  ($/:= #js.img.style.left "0")
  (#js.document.body.appendChild #js.img)

  (define x start-x)
  (define y start-y)

  ; TODO these should be fetched dynamically,
  ; but typical javascript methods produced 0
  (define w 100)
  (define h 100)

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

