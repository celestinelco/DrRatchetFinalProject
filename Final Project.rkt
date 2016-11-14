;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Final Project|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DR RATCHET WE OUT HERE
(require rsound)
(require 2htdp/image)
(require 2htdp/universe)

;; a ws is
;; - (make-ws keyLastPressed)
;; where keyLastPressed is a String representing the last key the user pressed
(define-struct ws [keyLastPressed])
(define INITIAL-STATE
  (make-ws "0"))

(define FR-RATE 44100)
(define PR-WIDTH 1400)
(define PR-HEIGHT 800)
(define SQ-KEY-SIZE 150)

(define BG (rectangle PR-WIDTH
                      PR-HEIGHT
                      "solid"
                      "black"))

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
;make structure for piano keys, so when piano key is called you can just play certain synth-note

;; piano = 1100x600
;(define piano (rectangle 1100 600 "solid" "white"))
(define piano (bitmap "piano.png"))

;; Image will be placed with midpoint as reference
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


(define (checkKey x y)
  (cond [(and (>= x 1100) (< x 1250))
               (cond [(and (>= y 200) (< y 350)) sqKey1]
                     [(and (>= y 350) (< y 500)) sqKey3]
                     [(and (>= y 500) (< y 650)) sqKey5]
                     [(and (>= y 650) (<= y 800)) sqKey7]
                     [else blankKey])]
         [(and (>= x 1250) (<= x 1400))
                  (cond [(and (>= y 200) (< y 350)) sqKey2]
                        [(and (>= y 350) (< y 500)) sqKey4]
                        [(and (>= y 500) (< y 650)) sqKey6]
                        [(and (>= y 650) (<= y 800)) sqKey8]
                        [else blankKey])]
         [else blankKey]))


(define (playKey key)
  (cond [(equal? key sqKey1) (play kick)]
        [(equal? key sqKey2) (play bassdrum)]
        [(equal? key sqKey3) (play o-hi-hat)]
        [(equal? key sqKey4) (play c-hi-hat-1)]
        [(equal? key sqKey5) (play clap-1)]
        [(equal? key sqKey6) (play crash-cymbal)]
        [(equal? key sqKey7) (play snare)]
        [(equal? key sqKey8) (play ding)]
        [else (play (silence 1))]))

(define (handle-mouse ws x y event)
  (cond [(string=? event "button-down")
         (both (playKey (checkKey x y))
               ws)]
        [else ws]))

(define (both a b) b)

(big-bang INITIAL-STATE
          [to-draw draw-keys]
          [on-mouse handle-mouse])