#lang racket/base

;; Card Integer Library
;; supplies decimal->card and card->decimal functions

(provide decimal->ternary)
(provide integer->card)
(provide card->decimal)
(provide set->sum)
(provide integer-set->sum)

(define (decimal->ternary int)
  (if (< int 3)
      (list (remainder int 3))
      (append (decimal->ternary (floor (/ int 3)))
              (list (remainder int 3)))))

(define (integer->card int)
  (cond ((< int 3)
         (append '(0 0 0)
                 (decimal->ternary int)))
        ((< int 9)
         (append '(0 0)
                 (decimal->ternary int)))
        ((< int 27)
         (append '(0)
                 (decimal->ternary int)))
        (else
         (decimal->ternary int))))

(define (card->decimal ternary)
  (if (null? ternary)
      0
      (+ (* (car ternary)
            (expt 3 (sub1 (length ternary))))
         (card->decimal (cdr ternary)))))
      
(define (set->sum lat)
  (for/sum ([i lat])
    (card->decimal i)))
    
(define (integer-set->sum lat)
  (for/sum ([i lat])
    i))