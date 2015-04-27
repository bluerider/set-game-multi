#lang racket

;; library to handle A/I operations

;; require the following modules
(require "game-library.rkt")
(require "user-library.rkt")
(require "dealer-library.rkt")
(require data/queue)

;; provide the following modules
(provide make-autobot)
(provide on-autobot?)
(provide toggle-autobot)
(provide time-delay)
(provide ghosting)

;; method is (procedure values)
;;   the first argument of method should be the autobot
(struct autobot user ([method #:mutable]))
       
;; produce an autobot (has its own method)
(define (make-autobot user-queue method)
  (letrec ([prot-robot (autobot (make-queue)
                                (make-queue)
                                (make-queue)
                                'autobot
                                #f)])
       ;; add starting time
       (add-time prot-robot)
       ;; activate the method by overloading field
       ;; method = (proc values)
       (set-autobot-method! prot-robot
                            (apply (car method)
                             (cons prot-robot (cdr method))))
       ;; add robot to the user queue
       (enqueue! user-queue prot-robot)
       ;; return the robot
       prot-robot))
       
;; toggle the robot's method on or off
(define toggle-autobot
  (lambda (autobot
           #:on? [on? (not (on-autobot? autobot))])
    ;; activate the thread if it's not on
    (if (false? on?)
        (thread-suspend (autobot-method autobot))
        (thread-resume (autobot-method autobot)))
    (on-autobot? autobot)))

;; is the autobot on?
(define (on-autobot? autobot)
  ;; find out if the robot's method thread is running
  (thread-running? (autobot-method autobot)))

;; pick a random set and check if it's a set
(define (get-set? robot dealer)
  (let ([pos-set (find-set (deal-cards dealer))])
       (if (false? pos-set)
           (if (false? (draw-3-cards dealer))
               (kill-thread (current-thread))
               #t)
           ((lambda () 
              (map (lambda (card)
                     (add-card robot card))
                   pos-set)
              (check-hand robot dealer))))))
              
;; robot should sleep and then find a set
(define (robot-wait robot dealer requested-time)
  (sleep requested-time)
  (get-set? robot dealer))
       
;; time delay method
(define (time-delay robot dealer requested-delay)
  ;; robot sleep -> find set -> robot sleep ..
  (define (robot-thread)
     (robot-wait robot dealer requested-delay)
     (robot-thread))
  ;; launch thread for method
  (if (zero? requested-delay)
      #f
      (thread
        (lambda ()
          ;; yield the thread
          (sleep 0)
          (robot-thread)))))
          
;; ghosting method
;;  wait the avg. amount of time it takes the user to find a set
(define (ghosting robot user dealer)
  ;; get the avg of user times in seconds
  (define (avg-times user)
    (if (= 1 (length (return-times user)))
        ;; return 14 seconds if not enough time data
        14
        (/ (- (last-time user)
              (start-time user))
           ;; divide by 2000 milliseconds
           2000)))
  ;; get the average of all avg-times
  (define (multi-user-avg-times user)
     (/ (for/sum ([i (queue->list user)]) (avg-times i))
        (queue-length user)))
  ;; robot sleep -> find set -> robot sleep ..
  (define (robot-thread proc)
     (robot-wait robot dealer (proc user))
     (robot-thread proc))
  ;; run the thread
  (thread
     (lambda ()
       ;; yield the thread
       (sleep 0)
       ;; check if multiple users were passed
       (if (queue? user)
           (robot-thread multi-user-avg-times)
           (robot-thread avg-times)))))