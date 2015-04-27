#lang racket

;; user library

;; provide the following modules
(provide (struct-out user))
(provide make-user)
(provide add-card)
(provide remove-card)
(provide add-set)
(provide hand-size)
(provide sets-size)
(provide draw-a-card)
(provide show-hand)
(provide show-and-empty-hand)
(provide return-times)
(provide start-time)
(provide last-time)
(provide add-time)
(provide get-sets)
(provide get-type)

;; require the following modules
(require data/queue)


;; structs for user and dealer
;;   each slot needs to contain a queue
(struct user (hand times sets type))

;; functions to make users
;; make a user
(define (make-user queue)
  (let ([prot-user (user (make-queue)
                         (make-queue)
			 (make-queue)
			 'user)])
       ;; add starting time
       (add-time prot-user)
       ;; add the user to the user queue
       (enqueue! queue prot-user)
       ;; return the prototype user
       prot-user))
       
;; functions to add values to users and dealers
;; add a card to the user-hand
(define (add-card user card)
   (enqueue! (user-hand user) card))
;; remove a card from the user-hand)
(define (remove-card user card)
   (queue-filter! (user-hand user)
                  (lambda (hand-card)
			  (false? (equal? hand-card card)))))
;; add a set to the user-sets
(define (add-set user set)
   (enqueue! (user-sets user) set))
;; return the length of the user-hand
(define (hand-size user)
   (queue-length (user-hand user)))
;; return the length of the user found-sets
(define (sets-size user)
   (queue-length (user-sets user)))
(define (get-sets user)
   (queue->list (user-sets user)))
;; draw 1 card from a user
(define (draw-a-card user)
   (dequeue! (user-hand user)))
;; return a list that contains the card's in the user's hand
(define (show-hand user)
   (queue->list (user-hand user)))
;; return a list that contain's the cards in the user's hand and empty it
(define (show-and-empty-hand user)
   (build-list (hand-size user) (lambda (int) (draw-a-card user))))
;; return all user times
(define (return-times user)
   (queue->list (user-times user)))
;; return the user start-time
(define (start-time user)
   (car (queue->list (user-times user))))
;; return the user's last card time
(define (last-time user)
   (last (queue->list (user-times user))))
;; add time to a user's queue
(define (add-time user)
   (enqueue! (user-times user) (current-milliseconds)))
;; return the type of user
(define (get-type user)
   (user-type user))