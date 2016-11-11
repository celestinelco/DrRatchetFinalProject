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

(define PR-WIDTH 1400)
(define PR-HEIGHT 800)
(define SQ-KEY-SIZE 25)


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
(define piano (rectangle 800 600 "solid" "white"))

; ...more sqKey(#'s)
(define sqKeyList
  (list
   sqKey1
   sqKey2
   sqKey3
   sqKey4
   sqKey5
   sqKey6
   sqKey7
   sqKey8))

(define (draw-keys ws)
  (both
  (place-image sqKey1
               1200 500
               BG)
  (place-image piano
               0 300
               BG)))
(define (both a b) b)


(big-bang INITIAL-STATE
          [to-draw draw-keys])