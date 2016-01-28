#lang racket/base

;; require the following modules
(require racket/draw)
(require "radix-library.rkt")

;; provide the following modules
(provide card->number)
(provide number->gif)
(provide card->gif)
(provide card->image)
(provide number->image)

;; package .gif cards with executable
(require racket/runtime-path)
(define-runtime-path set-cards "set-cards")
(require racket/flonum)

;; (number, color, shape, filling)
;; (1,2,3)
;; (red purple green)
;; (wavy diamond pill)
;; (solid striped empty)
;; get the gif image for the card
;; convert card to a number
(define (card->number card)
  (card->decimal card))

;; convert number to gif path
(define (number->gif number)
   (string-append (path->string set-cards)
                  (string-append "/"
                    (string-append 
                       (number->string 
                          (if (flonum? number)
                              (fl->exact-integer number)
                              number))
                       ".png"))))

;; convert number to image
(define (number->image number scale)
  (read-bitmap (number->gif number)
               #:backing-scale scale))
  
;; convert card to gif path
(define (card->gif card)
  (number->gif (card->number card)))

;; convert card to image
(define (card->image card scale)
  (read-bitmap (card->gif card)
               #:backing-scale scale))