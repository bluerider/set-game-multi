#lang racket/base

;; library to handle dealer operations

;; provide the following modules
(provide (struct-out dealer))
(provide make-dealer)
(provide check-hand)
(provide update-number-of-shown-cards)
(provide get-number-of-shown-cards)
(provide deal-cards)
(provide draw-3-cards)

;; load required modules
(require "game-library.rkt")
(require "user-library.rkt")
(require data/queue)
(require racket/list)

;; create the dealer struct from the user struct
;;  has an extra parameter for the # of cards to show from the top of the deck
(struct dealer user (number-of-shown-cards))

;; make a dealer
(define (make-dealer deck)
  (let ([prot-dealer (dealer (make-queue)
                             (make-queue)
                             (make-queue)
                             'dealer
                             (make-queue))])
       ;; add starting time
       (add-time prot-dealer)
       ;; add shuffled deck into dealer hand
       (for-each (lambda (card)
                         (add-card prot-dealer card))
		 deck)
       ;; return the dealer
       prot-dealer))

;; check a hand for sets
(define (check-hand user dealer)
	;; get the user hand
  (let ([hand-list (show-and-empty-hand user)])
	   ;; check if the user's hand contains a set
       (if (set? hand-list)
           (begin
            ;; add the found set to the user's and dealer's found-set queue
            (add-set user hand-list)
            (add-set dealer hand-list)
            (add-time user)
            (add-time dealer)
            ;; remove the card from the dealer's deck
            (for-each (lambda (card)
                        (remove-card dealer card))
                      hand-list)
            ;; reduce the # of cards by 3 if there are more than 12 cards
            ;;  or if the dealer has at most 12 cards left
            (let ([num-cards (get-number-of-shown-cards dealer)])
                 (if (or (< 12 num-cards) (> 12 (length (show-hand dealer))))
                     (update-number-of-shown-cards dealer (- num-cards 3))
                     #t)))
	   #f)))

;; update the dealer's # of shown cards
(define (update-number-of-shown-cards dealer int)
   (let ([dealer-hand (dealer-number-of-shown-cards dealer)])
        (enqueue! dealer-hand int)
        (if (<= 2 (queue-length dealer-hand))
            (dequeue! dealer-hand)
            #t)))
	   
;; get the number of cards the dealer is supposed to show
(define (get-number-of-shown-cards dealer)
   (let ([num-list (queue->list (dealer-number-of-shown-cards dealer))])
        (if (null? num-list)
            0
            (car num-list))))

;; tell the dealer to draw 3 cards
(define (draw-3-cards dealer)
  (let ([number-available (- (length (show-hand dealer))
                             (get-number-of-shown-cards dealer))])
   (cond ((<= 3 number-available)
          (update-number-of-shown-cards dealer
            (+ 3 (get-number-of-shown-cards dealer))))
         ((zero? number-available) #f)
         (else
          (update-number-of-shown-cards dealer
            (length (show-hand dealer)))))))
              
;; generate a snapshot of the topmost cards in the dealer-deck
;; need a queue containing the dealer's # cards shown
(define (deal-cards dealer)
   (take (show-hand dealer)
         (get-number-of-shown-cards dealer)))