#lang racket/base

(require mred
         racket/match)

(provide my-get-file)

;; allow the user to select a file; return
;; its path as a string, or #f if no selection
;; is made
(define (my-get-file starting-dir)
  (match (get-file "Pick a song!"
                   #f
                   starting-dir)
    [#f #f]
    [other (path->string other)]))