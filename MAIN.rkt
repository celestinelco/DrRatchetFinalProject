#lang racket
; Dr Ratchet Programmable MIDI Sound Controller
; CPE 123 Fall 2016
; By: Celestine Co, Gina Filangeri, Josiah Pang, & Sharmayne Tanedo
; DR RATCHET WE OUT HERE

; ===============================================================================
; ==== Setup ====================================================================
; ===============================================================================

; Library stuff
(require rsound)
(require rsound/piano-tones)
(require 2htdp/image)
(require 2htdp/universe)
(require "my-get-file.rkt")

;; a ws is
;; - (make-ws keyLastPressed slider-frac-x track-volume end-frame)
;; Where keyLastPressed is a String representing the last Key the user pressed
;; either by pressing the corresponding keyboard button OR the region mapped
;; in the scene corresponding to the Key
;; Keys are either piano keys or programmable MIDI keys
;; And track-volume is
;; the volume of the backtrack expressed through a decimal value between 0.0 and 1.0
;; And slider-frac-x represents the percentage through the backtrack is currently playing
;; expressed through a decimal value between 0.0 and 1.0
;; And end-frame assists the pstream-queue function to allow sound manipulation in the bstream
(define-struct ws [keyLastPressed slider-frac-x track-volume end-frame los total-frames])
(define INITIAL-STATE
  (make-ws "0" 0.0 0.5 0 '() 0)) ; No key is pressed
      
;(check-expect (make-ws pk1)
;             (make-ws (piano-tone 48)))


; Constant definitions
(define FR-RATE 44100)
(define PR-WIDTH 1400)
(define PR-HEIGHT 800)
(define VOL-SLIDER-HEIGHT 110)
(define VOL-MID-X 725)
(define VOL-SLIDER (bitmap "Resources/slider.png"))
(define X-SLIDER-WIDTH 290)
(define X-SLIDER-MID-Y 145) ; 1105 < x < 1395
(define X-SLIDER (bitmap "Resources/slidervert.png"))
(define SQ-KEY-SIZE 150)

(define rstream (make-pstream)) ;for keys
(define bstream (make-pstream)) ;for backtrack

(define BG (rectangle PR-WIDTH
                      PR-HEIGHT
                      "solid"
                      "black"))

;(define song
;  (resample-to-rate
;   FRAME-RATE
;   (rs-read/clip "Backtracks/beat1.wav"
;                 0
;                 (* 60 FRAME-RATE))))

;; how long should each queued segment be, in seconds?
(define PLAY-SECONDS 1/20)
;; .. in frames?
(define PLAY-FRAMES (* PLAY-SECONDS FR-RATE))
;;; .. as a fraction of the slider?
;(define PLAY-POSNFRAC (/ PLAY-SECONDS (/ (rs-frames (first list-of-songs)) FR-RATE)))
;; how long should the big-bang ticks be?
(define TICK-LEN 1/40)
;; the longest lead time for which we'll queue the next sound
(define MAX-QUEUE-INTERVAL (* 3/80 FR-RATE))
;; is it time to play the next chunk, yet?
(define (time-to-play? end-frame cur-frame)
  (< (- end-frame cur-frame) MAX-QUEUE-INTERVAL))

; Puts something into list
(define (put x lok)
  (cons lok (cons x '())))

; ===============================================================================
; ==== MIDI Key Stuff ===========================================================
; ===============================================================================

; Programmable MIDI key definitions
; sets a size and color of the midi key
(define sqKey1 (bitmap "Resources/Keys/-.png"))
(define sqKey2 (rectangle SQ-KEY-SIZE
                          SQ-KEY-SIZE
                          "solid"
                          "PapayaWhip"))
(define sqKey3 (rectangle SQ-KEY-SIZE
                          SQ-KEY-SIZE
                          "solid"
                          "LemonChiffon"))
(define sqKey4 (rectangle SQ-KEY-SIZE
                          SQ-KEY-SIZE
                          "solid"
                          "PaleGoldenrod"))
(define sqKey5 (rectangle SQ-KEY-SIZE
                          SQ-KEY-SIZE
                          "solid"
                          "Seashell"))
(define sqKey6 (rectangle SQ-KEY-SIZE
                          SQ-KEY-SIZE
                          "solid"
                          "Cornsilk"))
(define sqKey7 (rectangle SQ-KEY-SIZE
                          SQ-KEY-SIZE
                          "solid"
                          "FloralWhite"))
(define sqKey8 (rectangle SQ-KEY-SIZE
                          SQ-KEY-SIZE
                          "solid"
                          "Linen"))
(define blankKey (rectangle SQ-KEY-SIZE
                            SQ-KEY-SIZE
                            "solid"
                            "white")) 

(define sqKeys (bitmap "Resources/beatboxes.png"))
(define topKeys (bitmap "Resources/top.png"))

;testing overlay keys
#;(define (draw-keys ws)
  (cond [(key=? "q") (place-image Q 55 350
(place-image VOL-SLIDER
               VOL-MID-X (+ 65 (* (ws-track-volume ws) VOL-SLIDER-HEIGHT))
  (place-image sqKeys
               1250 500
  (place-image topKeys
               700 100
  (place-image piano
               550 500
  BG)))))]
                               
        [else                       
  (place-image VOL-SLIDER
               VOL-MID-X (+ 65 (* (ws-track-volume ws) VOL-SLIDER-HEIGHT))
  (place-image sqKeys
               1250 500
  (place-image topKeys
               700 100
  (place-image piano
               550 500
  BG))))]))
              

(define (draw-keys ws)
  (place-image VOL-SLIDER
               VOL-MID-X (+ 65 (* (ws-track-volume ws) VOL-SLIDER-HEIGHT))
  (place-image X-SLIDER
               (+ 1105 (* (ws-slider-frac-x ws) X-SLIDER-WIDTH)) X-SLIDER-MID-Y
  (place-image sqKeys
               1250 500
  (place-image topKeys
               700 100
  (place-image piano
               550 500
               BG))))))

; ===============================================================================
; ==== Piano Stuff ==============================================================
; ===============================================================================

; Abstraction for creating piano key definitions
(define (pknum pitch)
  (rs-scale 2.0 (piano-tone pitch)))

; Defines structures for piano keys to be called later
; pitch of the piano keys
(define pk1 (pknum 48))
(define pk2 (pknum 49))
(define pk3 (pknum 50))
(define pk4 (pknum 51))
(define pk5 (pknum 52))
(define pk6 (pknum 53))
(define pk7 (pknum 54))
(define pk8 (pknum 55))
(define pk9 (pknum 56))
(define pk10 (pknum 57))
(define pk11 (pknum 58))
(define pk12 (pknum 59))
(define pk13 (pknum 60)) ; Middle C
(define pk14 (pknum 61))
(define pk15 (pknum 62))
(define pk16 (pknum 63))
(define pk17 (pknum 64))
(define pk18 (pknum 65))
(define pk19 (pknum 66))
(define pk20 (pknum 67))
(define pk21 (pknum 68))
(define pk22 (pknum 69))
(define pk23 (pknum 70))
(define pk24 (pknum 71))
(define pk25 (pknum 72))
(define pk26 (pknum 73))
(define pk27 (pknum 74))
(define pk28 (pknum 75))
(define pk29 (pknum 76))
(define pk30 (pknum 77))
(define pk31 (pknum 78))
(define pk32 (pknum 79))
(define pk33 (pknum 80))
(define pk34 (pknum 81))
(define pk35 (pknum 82))

(define chooseFileKey ding)
(define stopKey kick)



; Defines our drawn piano section
; piano = 1100x600
;(define piano (rectangle 1100 600 "solid" "white"))
(define piano (bitmap "Resources/piano.png")) ; Replaces drawn image with static image

; Checks to see which piano key was pressed
; defines the range of the keys
; number number -> what key it signifies
(define (checkKey x y)
  (cond [(and (and (>= x 800) (< x 950)) (and (>= y 50) (< y 200))) chooseFileKey]
        [(and (and (>= x 950) (< x 1100)) (and (>= y 50) (< y 200))) stopKey]
        [(and (>= x 1100) (< x 1250)) ;left vertical range
               (cond [(and (>= y 200) (< y 350)) sqKey1] ;checks h. range of kick key
                     [(and (>= y 350) (< y 500)) sqKey3] ;checks h. range of o-hi-hat key
                     [(and (>= y 500) (< y 650)) sqKey5] ;checks h. range of clap1 key
                     [(and (>= y 650) (<= y 800)) sqKey7] ;checks h. range of the snare key
                     [else blankKey])]
         [(and (>= x 1250) (<= x 1400)) ;right vertical range
                  (cond [(and (>= y 200) (< y 350)) sqKey2] ;checks h. range of bassdrum key
                        [(and (>= y 350) (< y 500)) sqKey4] ;checks h. range of c-hi-hat-1 key
                        [(and (>= y 500) (< y 650)) sqKey6] ;checks h. range of crash-cymbal key
                        [(and (>= y 650) (<= y 800)) sqKey8] ;checks h. range of ding key
                        [else blankKey])]
         [(and (>= y 200) (< y 350)) ;first row of piano keyboard - black keys and partial white keys
          (cond [(and (>= x 0) (< x 55)) pk1] ;vertical range of C key
                [(and (>= x 55) (< x 165)) pk2] ;vertical range of C sharp key
                [(and (>= x 165) (< x 275)) pk4] ;vertical range of D sharp key
                [(and (>= x 275) (< x 330)) pk5] ;vertical range of E key
                [(and (>= x 330) (< x 385)) pk6] ;vertical range of F key
                [(and (>= x 385) (< x 495)) pk7] ;vertical range of F sharp key
                [(and (>= x 495) (< x 605)) pk9] ;vertical range of A flat key
                [(and (>= x 605) (< x 715)) pk11] ;vertical range of B flat key
                [(and (>= x 715) (< x 770)) pk12] ;vertical range of B key
                [(and (>= x 770) (< x 825)) pk13] ;vertical range of C key
                [(and (>= x 825) (< x 935)) pk14] ;vertical range of C sharp key
                [(and (>= x 935) (< x 1045)) pk16] ;vertical range of D sharp key
                [(and (>= x 1045) (< x 1100)) pk17] ;vertical range of E key
                [else blankKey])]
         [(and (>= y 350) (< y 500)) ;second row of piano keyboard - just white keys
          (cond [(and (>= x 0) (< x 110)) pk1] ;vertical range of C key
                [(and (>= x 110) (< x 220)) pk3] ;vertical range of D key
                [(and (>= x 220) (< x 330)) pk5] ;vertical range of E key
                [(and (>= x 330) (< x 440)) pk6] ;vertical range of F key
                [(and (>= x 440) (< x 550)) pk8] ;vertical range of G key
                [(and (>= x 550) (< x 660)) pk10] ;vertical range of A key
                [(and (>= x 660) (< x 770)) pk12] ;vertical range of B key
                [(and (>= x 770) (< x 880)) pk13] ;vertical range of C key
                [(and (>= x 880) (< x 990)) pk15] ;vertical range of D key
                [(and (>= x 990) (< x 1100)) pk17] ;vertical range of E key
                [else blankKey])]
         [(and (>= y 500) (< y 650)) ;third row of piano key board - black and white keys
          (cond [(and (>= x 0) (< x 55)) pk18]  ;vertical range of F key
                [(and (>= x 55) (< x 165)) pk19]  ;vertical range of F sharp key
                [(and (>= x 165) (< x 275)) pk21] ;vertical range of A flat key
                [(and (>= x 275) (< x 385)) pk23] ;vertical range of B flat key
                [(and (>= x 385) (< x 440)) pk24] ;vertical range of B key
                [(and (>= x 440) (< x 495)) pk25] ;vertical range of C key
                [(and (>= x 495) (< x 605)) pk26] ;vertical range of C sharp key
                [(and (>= x 605) (< x 715)) pk28] ;vertical range of D sharp key
                [(and (>= x 715) (< x 770)) pk29] ;vertical range of E key
                [(and (>= x 770) (< x 825)) pk30] ;vertical range of F key
                [(and (>= x 825) (< x 935)) pk31] ;vertical range of F sharp key
                [(and (>= x 935) (< x 1045)) pk33] ;vertical range of A flat key
                [(and (>= x 1045) (< x 1100)) pk35] ;vertical range of B flat key
                [else blankKey])]
         [(and (>= y 650) (<= y 800)) ;last row of piano key board - just white keys
          (cond [(and (>= x 0) (< x 110)) pk18]  ;vertical range of F key
                [(and (>= x 110) (< x 220)) pk20]  ;vertical range of G key
                [(and (>= x 220) (< x 330)) pk22] ;vertical range of A key
                [(and (>= x 330) (< x 440)) pk24] ;vertical range of B key
                [(and (>= x 440) (< x 550)) pk25] ;vertical range of C key
                [(and (>= x 550) (< x 660)) pk27] ;vertical range of D key
                [(and (>= x 660) (< x 770)) pk29] ;vertical range of E key
                [(and (>= x 770) (< x 880)) pk30] ;vertical range of F key
                [(and (>= x 880) (< x 990)) pk32] ;vertical range of G key
                [(and (>= x 990) (< x 1100)) pk34] ;vertical range of A key
                [else blankKey])]
         [else blankKey]))
;(check-expect (checkKey 166 400) pk3)
;(check-expect (checkKey 940 600) pk33)
;(check-expect (checkKey 4 770) pk18)
;(check-expect (checkKey 1300 200) sqKey2)


; Plays sound when key is pressed
; position on ws -> noise
(define (playKey key ws)
  (cond [(equal? key sqKey1) (both (pstream-play rstream kick) ws)]
        [(equal? key sqKey2) (both (pstream-play rstream bassdrum) ws)]
        [(equal? key sqKey3) (both (pstream-play rstream o-hi-hat) ws)]
        [(equal? key sqKey4) (both (pstream-play rstream c-hi-hat-1) ws)]
        [(equal? key sqKey5) (both (pstream-play rstream clap-1) ws)]
        [(equal? key sqKey6) (both (pstream-play rstream crash-cymbal) ws)]
        [(equal? key sqKey7) (both (pstream-play rstream snare) ws)]
        [(equal? key sqKey8) (both (pstream-play rstream ding) ws)]
        [(equal? key pk1) (both (pstream-play rstream pk1) ws)]
        [(equal? key pk2) (both (pstream-play rstream pk2) ws)]
        [(equal? key pk3) (both (pstream-play rstream pk3) ws)]
        [(equal? key pk4) (both (pstream-play rstream pk4) ws)]
        [(equal? key pk5) (both (pstream-play rstream pk5) ws)]
        [(equal? key pk6) (both (pstream-play rstream pk6) ws)]
        [(equal? key pk7) (both (pstream-play rstream pk7) ws)]
        [(equal? key pk8) (both (pstream-play rstream pk8) ws)]
        [(equal? key pk9) (both (pstream-play rstream pk9) ws)]
        [(equal? key pk10) (both (pstream-play rstream pk10) ws)]
        [(equal? key pk11) (both (pstream-play rstream pk11) ws)]
        [(equal? key pk12) (both (pstream-play rstream pk12) ws)]
        [(equal? key pk13) (both (pstream-play rstream pk13) ws)]
        [(equal? key pk14) (both (pstream-play rstream pk14) ws)]
        [(equal? key pk15) (both (pstream-play rstream pk15) ws)]
        [(equal? key pk16) (both (pstream-play rstream pk16) ws)]
        [(equal? key pk17) (both (pstream-play rstream pk17) ws)]
        [(equal? key pk18) (both (pstream-play rstream pk18) ws)]
        [(equal? key pk19) (both (pstream-play rstream pk19) ws)]
        [(equal? key pk20) (both (pstream-play rstream pk20) ws)]
        [(equal? key pk21) (both (pstream-play rstream pk21) ws)]
        [(equal? key pk22) (both (pstream-play rstream pk22) ws)]
        [(equal? key pk23) (both (pstream-play rstream pk23) ws)]
        [(equal? key pk24) (both (pstream-play rstream pk24) ws)]
        [(equal? key pk25) (both (pstream-play rstream pk25) ws)]
        [(equal? key pk26) (both (pstream-play rstream pk26) ws)]
        [(equal? key pk27) (both (pstream-play rstream pk27) ws)]
        [(equal? key pk28) (both (pstream-play rstream pk28) ws)]
        [(equal? key pk29) (both (pstream-play rstream pk29) ws)]
        [(equal? key pk30) (both (pstream-play rstream pk30) ws)]
        [(equal? key pk31) (both (pstream-play rstream pk31) ws)]
        [(equal? key pk32) (both (pstream-play rstream pk32) ws)]
        [(equal? key pk33) (both (pstream-play rstream pk33) ws)]
        [(equal? key pk34) (both (pstream-play rstream pk34) ws)]
        [(equal? key pk35) (both (pstream-play rstream pk35) ws)]
        [(equal? key chooseFileKey)
         (new-ws-adjust-los-total-frames ws (my-get-file "Backtracks"))]
        [(equal? key stopKey)
         (make-ws (ws-keyLastPressed ws)
                  (ws-slider-frac-x ws)
                  (ws-track-volume ws)
                  (ws-end-frame ws)
                  '()
                  0)]
        [else (both (pstream-play rstream (silence 1)) ws)]))

;returns ws with los and total-frames adjusted
(define (new-ws-adjust-los-total-frames ws path)
  (make-ws (ws-keyLastPressed ws)
                  0
                  0.5
                  0
                  (append (ws-los ws)
                          (cons (rs-read/clip path 0 (rs-read-frames path)) '()))
                  (rs-read-frames path)))

;(check-expect (playKey sqKey2) (pstream-play rstream bassdrum))
;(check-expect (playKey pk32) (pstream-play rstream pk32))

; Key highlight definitions
(define Q (bitmap "Resources/Keys/Q.png"))
(define W (bitmap "Resources/Keys/W.png"))
(define E (bitmap "Resources/Keys/E.png"))
(define R (bitmap "Resources/Keys/R.png"))
(define T (bitmap "Resources/Keys/T.png"))
(define Y (bitmap "Resources/Keys/Y.png"))
(define U (bitmap "Resources/Keys/U.png"))
(define I (bitmap "Resources/Keys/I.png"))
(define O (bitmap "Resources/Keys/O.png"))
(define P (bitmap "Resources/Keys/P.png"))
(define two (bitmap "Resources/Keys/2.png"))
(define three (bitmap "Resources/Keys/3.png"))
(define five (bitmap "Resources/Keys/5.png"))
(define six (bitmap "Resources/Keys/6.png"))
(define seven (bitmap "Resources/Keys/7.png"))
(define nine (bitmap "Resources/Keys/9.png"))
(define zero (bitmap "Resources/Keys/0.png"))
(define Z (bitmap "Resources/Keys/Z.png"))
(define X (bitmap "Resources/Keys/X.png"))
(define C (bitmap "Resources/Keys/C.png"))
(define V (bitmap "Resources/Keys/V.png"))
(define B (bitmap "Resources/Keys/B.png"))
(define N (bitmap "Resources/Keys/N.png"))
(define M (bitmap "Resources/Keys/M.png"))
(define comma (bitmap "Resources/Keys/,.png"))
(define period (bitmap "Resources/Keys/..png"))
(define backslash (bitmap "Resources/Keys/backslash.png"))
(define S (bitmap "Resources/Keys/S.png"))
(define D (bitmap "Resources/Keys/D.png"))
(define F (bitmap "Resources/Keys/F.png"))
(define H (bitmap "Resources/Keys/H.png"))
(define J (bitmap "Resources/Keys/J.png"))
(define L (bitmap "Resources/Keys/L.png"))
(define semicolon (bitmap "Resources/Keys/;.png"))
(define apostrophe (bitmap "Resources/Keys/'.png"))
(define minus (bitmap "Resources/Keys/-.png"))
(define equal (bitmap "Resources/Keys/=.png"))
(define lbracket (bitmap "Resources/Keys/[.png"))
(define rbracket (bitmap "Resources/Keys/].png"))
(define up (bitmap "Resources/Keys/up.png"))
(define down (bitmap "Resources/Keys/down.png"))
(define left (bitmap "Resources/Keys/left.png"))
(define right (bitmap "Resources/Keys/right.png"))

; ===============================================================================
; ==== Additional Stuff =========================================================
; ===============================================================================

; Used to play a sound and return a world state
(define (both a b) b)

; Defines mouse handler
; Checks to see which key was clicked and then both
; plays the key and returns a world state
(define (handle-mouse ws x y event)
  (cond [(string=? event "button-down")
         (playKey (checkKey x y) ws)]
        [(string=? event "drag")
         (cond [(and (and (>= x 650) (< x 800)) (and (>= y 65) (< y 175)))
                (make-ws (ws-keyLastPressed ws)
                         (ws-slider-frac-x ws)
                         (/ (- y 65) VOL-SLIDER-HEIGHT)
                         (ws-end-frame ws)
                         (ws-los ws)
                         (ws-total-frames ws))]
               [(and (and (>= x 1105) (< x 1395)) (and (>= y 100) (< y 200)))
                (make-ws (ws-keyLastPressed ws)
                         (/ (- x 1105) X-SLIDER-WIDTH)
                         (ws-track-volume ws)
                         (ws-end-frame ws)
                         (ws-los ws)
                         (ws-total-frames ws))]
               [else ws])]
        [else ws]))

; Play key check abstraction
(define (play-key ws pk)
  (both (playKey pk ws) ws))

; Defines key handler
; Checks to see which key was pressed and both plays the
; key and returns a world state
; key -> noise and world state
(define (handle-key ws key)
  (cond [(key=? key "q") (play-key ws pk1)]
        [(key=? key "2") (play-key ws pk2)]
        [(key=? key "w") (play-key ws pk3)]
        [(key=? key "3") (play-key ws pk4)]
        [(key=? key "e") (play-key ws pk5)]
        [(key=? key "r") (play-key ws pk6)]
        [(key=? key "5") (play-key ws pk7)]
        [(key=? key "t") (play-key ws pk8)]
        [(key=? key "6") (play-key ws pk9)]
        [(key=? key "y") (play-key ws pk10)]
        [(key=? key "7") (play-key ws pk11)]
        [(key=? key "u") (play-key ws pk12)]
        [(key=? key "i") (play-key ws pk13)]
        [(key=? key "9") (play-key ws pk14)]
        [(key=? key "o") (play-key ws pk15)]
        [(key=? key "0") (play-key ws pk16)]
        [(key=? key "p") (play-key ws pk17)]
        [(key=? key "z") (play-key ws pk18)]
        [(key=? key "s") (play-key ws pk19)]
        [(key=? key "x") (play-key ws pk20)]
        [(key=? key "d") (play-key ws pk21)]
        [(key=? key "c") (play-key ws pk22)]
        [(key=? key "f") (play-key ws pk23)]
        [(key=? key "v") (play-key ws pk24)]
        [(key=? key "b") (play-key ws pk25)]
        [(key=? key "h") (play-key ws pk26)]
        [(key=? key "n") (play-key ws pk27)]
        [(key=? key "j") (play-key ws pk28)]
        [(key=? key "m") (play-key ws pk29)]
        [(key=? key ",") (play-key ws pk30)]
        [(key=? key "l") (play-key ws pk31)]
        [(key=? key ".") (play-key ws pk32)]
        [(key=? key ";") (play-key ws pk33)]
        [(key=? key "/") (play-key ws pk34)]
        [(key=? key "'") (play-key ws pk35)]
        [(key=? key "-") (play-key ws sqKey1)]
        [(key=? key "=") (play-key ws sqKey2)]
        [(key=? key "[") (play-key ws sqKey3)]
        [(key=? key "]") (play-key ws sqKey4)]
        [(key=? key "up") (play-key ws sqKey5)]
        [(key=? key "down") (play-key ws sqKey6)]
        [(key=? key "left") (play-key ws sqKey7)]
        [(key=? key "right") (play-key ws sqKey8)]
        [else ws]))


;; Queue up the next fragment
(define (queue-next-fragment songFr volume frameToPlay ws)
  (pstream-queue bstream
                 (rs-scale volume (clip (first (ws-los ws)) songFr (+ songFr PLAY-FRAMES))) frameToPlay))

;; if it's time, queue up the next section
;; of the song
(define (tick-fun ws)
  (cond [(empty? (ws-los ws)) ws]
        [else (cond [(reached-end? ws)
                     (make-ws (ws-keyLastPressed ws)
                              0.0
                              (ws-track-volume ws)
                              0
                              (ws-los ws)
                              (ws-total-frames ws))]
                    [else (cond [(time-to-play? (ws-end-frame ws)
                                                (pstream-current-frame bstream))
                                 (both
                                  (queue-next-fragment
                                   (round (* (ws-slider-frac-x ws)
                                             (rs-frames (first (ws-los ws)))))
                                   (- 1 (ws-track-volume ws))
                                   (ws-end-frame ws)
                                   ws)
                                  (make-ws (ws-keyLastPressed ws)
                                           (+ (ws-slider-frac-x ws) (/ PLAY-SECONDS (/ (rs-frames (first (ws-los ws))) FR-RATE)))
                                           (ws-track-volume ws)
                                           (+ (ws-end-frame ws) PLAY-FRAMES)
                                           (ws-los ws)
                                           (ws-total-frames ws))
                                  )]
                                [else ws])])]))

(define (reached-end? ws)
  (cond [(> (+ (round (* (ws-slider-frac-x ws) (rs-frames (first (ws-los ws)))))
               PLAY-FRAMES)
            (ws-total-frames ws))
         #true]
        [else #false]))
         

; Big Bang stuff
;creates the world
(big-bang INITIAL-STATE
          [to-draw draw-keys]
          [on-mouse handle-mouse]
          [on-key handle-key]
          [on-tick tick-fun TICK-LEN]
          )
                     
                        