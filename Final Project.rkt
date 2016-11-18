;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Final Project|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; a ws is
;; - (make-ws keyLastPressed)
;; Where keyLastPressed is a String representing the last Key the user pressed
;; either by pressing the corresponding keyboard button OR the region mapped
;; in the scene corresponding to the Key
;; Keys are either piano keys or programmable MIDI keys
(define-struct ws [keyLastPressed])
(define INITIAL-STATE
  (make-ws "0")) ; No key is pressed
      
(check-expect (make-ws pk1)
              (make-ws (piano-tone 48)))


; Constant definitions
(define FR-RATE 44100)
(define PR-WIDTH 1400)
(define PR-HEIGHT 800)
(define SQ-KEY-SIZE 150)

(define BG (rectangle PR-WIDTH
                      PR-HEIGHT
                      "solid"
                      "black"))

; ===============================================================================
; ==== MIDI Key Stuff ===========================================================
; ===============================================================================

; Programmable MIDI key definitions
; set a size and color of the midi key
(define sqKey1 (rectangle SQ-KEY-SIZE
                          SQ-KEY-SIZE
                          "solid"
                          "MistyRose"))
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

; Draws programmable MIDI keys based on their midpoints
(define (draw-keys ws)
  (place-image sqKey7
               1175 725
  (place-image sqKey8
               1325 725
  (place-image sqKey5
               1175 575
  (place-image sqKey6
               1325 575
  (place-image sqKey3
               1175 425
  (place-image sqKey4
               1325 425
  (place-image sqKey1
               1175 275
  (place-image sqKey2
               1325 275
  (place-image piano
               550 500
               BG))))))))))

; ===============================================================================
; ==== Piano Stuff ==============================================================
; ===============================================================================

; Defines structures for piano keys to be called later
; pitch of the piano keys
(define pk1 (piano-tone 48))
(define pk2 (piano-tone 49))
(define pk3 (piano-tone 50))
(define pk4 (piano-tone 51))
(define pk5 (piano-tone 52))
(define pk6 (piano-tone 53))
(define pk7 (piano-tone 54))
(define pk8 (piano-tone 55))
(define pk9 (piano-tone 56))
(define pk10 (piano-tone 57))
(define pk11 (piano-tone 58))
(define pk12 (piano-tone 59))
(define pk13 (piano-tone 60)) ; MIDDLE C
(define pk14 (piano-tone 61))
(define pk15 (piano-tone 62))
(define pk16 (piano-tone 63))
(define pk17 (piano-tone 64))
(define pk18 (piano-tone 65))
(define pk19 (piano-tone 66))
(define pk20 (piano-tone 67))
(define pk21 (piano-tone 68))
(define pk22 (piano-tone 69))
(define pk23 (piano-tone 70))
(define pk24 (piano-tone 71))
(define pk25 (piano-tone 72))
(define pk26 (piano-tone 73))
(define pk27 (piano-tone 74))
(define pk28 (piano-tone 75))
(define pk29 (piano-tone 76))
(define pk30 (piano-tone 77))
(define pk31 (piano-tone 78))
(define pk32 (piano-tone 79))
(define pk33 (piano-tone 80))
(define pk34 (piano-tone 81))
(define pk35 (piano-tone 82))

; Defines our drawn piano section
; piano = 1100x600
;(define piano (rectangle 1100 600 "solid" "white"))
(define piano (bitmap "Resources/piano.png")) ; Replaced drawn image with static image

; Checks to see which piano key was pressed
; defines the range of the keys
; number number -> what key it signifies
(define (checkKey x y)
  (cond [(and (>= x 1100) (< x 1250)) ;left vertical range
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
         [(and (>= y 200) (< y 300)) ;first row of piano keyboard - black keys and partial white keys
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
(check-expect (checkKey 166 400) pk3)
(check-expect (checkKey 940 600) pk33)
(check-expect (checkKey 4 770) pk18)
(check-expect (checkKey 1300 200) sqKey2)


; Plays sound when key is pressed
; position on ws -> noise
(define rstream (make-pstream))
(define (playKey key)
  (cond [(equal? key sqKey1) (pstream-play rstream kick)]
        [(equal? key sqKey2) (pstream-play rstream bassdrum)]
        [(equal? key sqKey3) (pstream-play rstream o-hi-hat)]
        [(equal? key sqKey4) (pstream-play rstream c-hi-hat-1)]
        [(equal? key sqKey5) (pstream-play rstream clap-1)]
        [(equal? key sqKey6) (pstream-play rstream crash-cymbal)]
        [(equal? key sqKey7) (pstream-play rstream snare)]
        [(equal? key sqKey8) (pstream-play rstream ding)]
        [(equal? key pk1) (pstream-play rstream pk1)]
        [(equal? key pk2) (pstream-play rstream pk2)]
        [(equal? key pk3) (pstream-play rstream pk3)]
        [(equal? key pk4) (pstream-play rstream pk4)]
        [(equal? key pk5) (pstream-play rstream pk5)]
        [(equal? key pk6) (pstream-play rstream pk6)]
        [(equal? key pk7) (pstream-play rstream pk7)]
        [(equal? key pk8) (pstream-play rstream pk8)]
        [(equal? key pk9) (pstream-play rstream pk9)]
        [(equal? key pk10) (pstream-play rstream pk10)]
        [(equal? key pk11) (pstream-play rstream pk11)]
        [(equal? key pk12) (pstream-play rstream pk12)]
        [(equal? key pk13) (pstream-play rstream pk13)]
        [(equal? key pk14) (pstream-play rstream pk14)]
        [(equal? key pk15) (pstream-play rstream pk15)]
        [(equal? key pk16) (pstream-play rstream pk16)]
        [(equal? key pk17) (pstream-play rstream pk17)]
        [(equal? key pk18) (pstream-play rstream pk18)]
        [(equal? key pk19) (pstream-play rstream pk19)]
        [(equal? key pk20) (pstream-play rstream pk20)]
        [(equal? key pk21) (pstream-play rstream pk21)]
        [(equal? key pk22) (pstream-play rstream pk22)]
        [(equal? key pk23) (pstream-play rstream pk23)]
        [(equal? key pk24) (pstream-play rstream pk24)]
        [(equal? key pk25) (pstream-play rstream pk25)]
        [(equal? key pk26) (pstream-play rstream pk26)]
        [(equal? key pk27) (pstream-play rstream pk27)]
        [(equal? key pk28) (pstream-play rstream pk28)]
        [(equal? key pk29) (pstream-play rstream pk29)]
        [(equal? key pk30) (pstream-play rstream pk30)]
        [(equal? key pk31) (pstream-play rstream pk31)]
        [(equal? key pk32) (pstream-play rstream pk32)]
        [(equal? key pk33) (pstream-play rstream pk33)]
        [(equal? key pk34) (pstream-play rstream pk34)]
        [(equal? key pk35) (pstream-play rstream pk35)]
        [else (pstream-play rstream (silence 1))]))

(check-expect (playKey sqKey2) (pstream-play rstream bassdrum))
(check-expect (playKey pk32) (pstream-play rstream pk32))
; ===============================================================================
; ==== Additional Stuff =========================================================
; ===============================================================================

(define (both a b) b)


; Defines mouse handler
; Checks to see which key was clicked and then both
; plays the key and returns a world state
(define (handle-mouse ws x y event)
  (cond [(string=? event "button-down")
         (both (playKey (checkKey x y))
               ws)]
        [else ws]))

; Defines key handler
; Checks to see which key was pressed and both plays the
; key and returns a world state
; key -> noise and world state
(define (handle-key ws key)
  (cond [(key=? key "q") (both (playKey pk1) ws)]
        [(key=? key "2") (both (playKey pk2) ws)]
        [(key=? key "w") (both (playKey pk3) ws)]
        [(key=? key "3") (both (playKey pk4) ws)]
        [(key=? key "e") (both (playKey pk5) ws)]
        [(key=? key "r") (both (playKey pk6) ws)]
        [(key=? key "5") (both (playKey pk7) ws)]
        [(key=? key "t") (both (playKey pk8) ws)]
        [(key=? key "6") (both (playKey pk9) ws)]
        [(key=? key "y") (both (playKey pk10) ws)]
        [(key=? key "7") (both (playKey pk11) ws)]
        [(key=? key "u") (both (playKey pk12) ws)]
        [(key=? key "i") (both (playKey pk13) ws)]
        [(key=? key "9") (both (playKey pk14) ws)]
        [(key=? key "o") (both (playKey pk15) ws)]
        [(key=? key "0") (both (playKey pk16) ws)]
        [(key=? key "p") (both (playKey pk17) ws)]
        [(key=? key "z") (both (playKey pk18) ws)]
        [(key=? key "s") (both (playKey pk19) ws)]
        [(key=? key "x") (both (playKey pk20) ws)]
        [(key=? key "d") (both (playKey pk21) ws)]
        [(key=? key "c") (both (playKey pk22) ws)]
        [(key=? key "f") (both (playKey pk23) ws)]
        [(key=? key "v") (both (playKey pk24) ws)]
        [(key=? key "b") (both (playKey pk25) ws)]
        [(key=? key "h") (both (playKey pk26) ws)]
        [(key=? key "n") (both (playKey pk27) ws)]
        [(key=? key "j") (both (playKey pk28) ws)]
        [(key=? key "m") (both (playKey pk29) ws)]
        [(key=? key ",") (both (playKey pk30) ws)]
        [(key=? key "l") (both (playKey pk31) ws)]
        [(key=? key ".") (both (playKey pk32) ws)]
        [(key=? key ";") (both (playKey pk33) ws)]
        [(key=? key "/") (both (playKey pk34) ws)]
        [(key=? key "'") (both (playKey pk35) ws)]
        [(key=? key "-") (both (playKey sqKey1) ws)]
        [(key=? key "=") (both (playKey sqKey2) ws)]
        [(key=? key "[") (both (playKey sqKey3) ws)]
        [(key=? key "]") (both (playKey sqKey4) ws)]
        [(key=? key "up") (both (playKey sqKey5) ws)]
        [(key=? key "down") (both (playKey sqKey6) ws)]
        [(key=? key "left") (both (playKey sqKey7) ws)]
        [(key=? key "right") (both (playKey sqKey8) ws)]
        [else ws]))

; Big Bang stuff
;creates the world
(big-bang INITIAL-STATE
          [to-draw draw-keys]
          [on-mouse handle-mouse]
          [on-key handle-key])