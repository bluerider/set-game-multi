#lang racket

;; Net dealer library

;; provide the following modules
(provide (struct-out net-dealer))
(provide vector->queue)
(provide dealer->net-dealer)
(provide net-dealer->dealer)

;; Load the following modules
(require "dealer-library.rkt")
(require "user-library.rkt")
(require racket/serialize)
(require data/queue)
(require net/websocket)

;; generate a serializable struct
(serializable-struct net-dealer (hand times sets number-of-shown-cards))

;; convert vector to queue
(define (vector->queue vector)
   (let ([queue (make-queue)])
        (map (lambda (atom)
                (enqueue! queue atom))
             (vector->list vector))
        ;; return queue
        queue))

;;  convert dealer to net-dealer
(define (dealer->net-dealer dealer)
  (let ([prot-dealer (net-dealer (vector (show-hand dealer))
                                 (vector (return-times dealer))
                                 (vector (get-sets dealer))
                                 (vector (get-number-of-shown-cards dealer)))])
       ;;return the net-dealer
       prot-dealer))

;; convert net-dealer to dealer
(define (net-dealer->dealer net-dealer)
  ;; deserialize net-dealer
  (define deserialized-dealer
     (deserialize net-dealer))
  ;; generate the dealer
  (let ([prot-dealer (dealer (vector->queue (net-dealer-hand deserialized-dealer))
                             (vector->queue (net-dealer-times deserialized-dealer))
                             (vector->queue (net-dealer-sets deserialized-dealer))
                             (vector->queue 
                                (net-dealer-number-of-shown-cards 
                                     deserialized-dealer)))])
       ;; return the dealer
       prot-dealer))

;; test out if we can convert between net-dealer and dealer
;; (let ([new-dealer (net-dealer->dealer
;;                      (serialize 
;;                         (dealer->net-dealer 
;;                           (make-dealer (shuffle-deck)))))])
;;      (show-hand new-dealer))