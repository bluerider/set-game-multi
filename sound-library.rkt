#lang racket/gui

;; provide the following modules
(provide found-set-sound)
(provide game-over-sound)
(provide game-music)

;; package all sound files with executable
(require racket/runtime-path)
(define-runtime-path sounds "sounds")

(define (play-music song async?)
  (play-sound (string-append
                 (path->string sounds)
                 (string-append "/" song))
              async?))

;; play sound when found set
(define (found-set-sound)
  (play-music "ding.wav" #f))
  
;; play game-over buzzer sound
(define (game-over-sound)
  (play-music "buzzer.wav" #f))
              
;; play background sound
(define (game-music)
   (thread (lambda ()
             (sleep 0)
             (play-music "muzak.wav" #f)
             (game-music))))