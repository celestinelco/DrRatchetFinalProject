;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname looper) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rsound)
(define (sec x) (* x 44100))
(define RECORD
  (record-sound (sec 2)))

(define RECORD-list
  (cons RECORD (cons (silence (sec 2)) '())))
(define (looper x)
  ( cond [(empty? x) (looper x)]
         [else (rs-overlay (play (first x)) (looper (rest x)))]))