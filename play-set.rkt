#lang racket/base

;; play the game of set with a gui

;; load the following modules
(require "window-library.rkt")
(require "game-library.rkt")
(require "user-library.rkt")
(require "robot-library.rkt")
(require "dealer-library.rkt")
(require "sound-library.rkt")
(require data/queue)
(require racket/class)
(require racket/bool)

;; generate a new-game with a given [struct] dealer
;;   a [queue] user
;;   a [boolean] fullscreen
;;   a [boolean] music?
(define gen-new-game
  (lambda (#:user-queue [user-queue (make-queue)]
           #:dealer [dealer (make-dealer (shuffle-deck))]
           #:fullscreen? [fullscreen? #f]
           #:music? [music? #t]
           #:notifications? [notifications? #t])
    (letrec (;; set up some windowing
             [set-master-window (gen-set-frame "Set Game" fullscreen?)]
             ;; set up the button panel
             [button-panel (gen-horizontal-panel set-master-window #t)]
             ;; set up the set panel
             [set-master-panel (gen-set-panel set-master-window)]
             ;; set up the master msg-panel
             [master-msg-panel (gen-horizontal-panel set-master-window #t)]
             ;; set up the msg-panel
             [msg-panel (gen-child-msg-panel master-msg-panel
                           "Howdy! Welcome to Set!"
                           #t)]
             ;; make a user if users weren't pass, else take the first one
             [user1 (if (queue-empty? user-queue)
                        (make-user user-queue)
                        (car user-queue))]
             ;; just set the dealer's name
             [dealer1 dealer])
            ;; tell the dealer to show 12 cards from the top of the deck
            ;;   if dealer doesnt' already have a number
            (if (zero? (get-number-of-shown-cards dealer1))
                (update-number-of-shown-cards dealer1 12)
                #t)
            ;; set up the set view pane
            (define user-view
                (let ([set-pane (view-sets-panel-refresh user1 "User Sets")])
                   (send set-pane show #f)
                   set-pane))
            (define dealer-view
                (let ([set-pane (view-sets-panel-refresh dealer1 "Dealer Sets")])
                   (send set-pane show #f)
                   set-pane))
	    ;; set the custodian to kill subprocesses (needed for play-sound)
	    (current-subprocess-custodian-mode 'kill)
            ;; set up some buttons
            (for-each (lambda (button-list)
                        (make-button button-panel (car button-list) 
                                                  (cadr button-list)
                                                  (caddr button-list)))
                 (list (list "New Game" (lambda (button) 
                                           (send (gen-new-game #:music? #f
                                                               #:notifications? notifications?
                                                               #:fullscreen? fullscreen?)
                                                 show #t))
                                        #f)
                       (list (if (false? fullscreen?)
                                  "Full Screen"
                                  "Shrink")
                         (lambda (button)
                           (send set-master-window fullscreen (not (send set-master-window is-fullscreened?)))
                           ;; update the button label
                           (send button set-label (if (not (false? (send set-master-window is-fullscreened?)))
                                                      "Full Screen"
                                                      "Shrink")))
                         #f)
                       (list "Draw 3 cards" 
                         (lambda (button)
                           (if (false? (draw-3-cards dealer1))
                                       (send-msg "No more cards to draw" msg-panel)
                                       #t))
                         #t)
                       (list "Hint"
                         (lambda (button)
                            (get-hint dealer1 msg-panel))
                         #f)
                       (list "User Sets" 
                         (lambda (button)
                           (send user-view show
                             (not (send user-view is-shown?))))
                         #f)
                       (list "Dealer Sets"
                         (lambda (button)
                           (send dealer-view show
                             (not (send dealer-view is-shown?))))
                         #f)
                       (list "Exit" (lambda (button)
                                       (exit)) #f)))
            ;; set up the AI slider
            ;;   slider value isn't the same between fullscreen and small panel
            (gen-robot-slider set-master-panel
               (lambda (slider-value)
                 (let ([adjusted-value (* 3 (- 12 slider-value))])
                   (cond ((zero? adjusted-value)
                          (begin
                             (map (lambda (user)
                                         ;; check if user is an autobot
                                    (if (equal? 'autobot (get-type user))
                                        ;; turn on autobot
                                        (toggle-autobot user #:on? #t)
                                        #f))
                                  (queue->list user-queue))
                             (send-msg "Turned on robots!" msg-panel)))
                         ((= 3 adjusted-value)
                          (begin
                             (map (lambda (user)
                                         ;; check if user is an autobot
                                    (if (equal? 'autobot (get-type user))
                                        ;; turn off autobot
                                        (toggle-autobot user #:on? #f)
                                        #f))
                                  (queue->list user-queue))
                             (send-msg "Turned off robots!" msg-panel)))
                         ((= 6 adjusted-value)
                          (begin
                             (send-msg "Fighting a ghost!" msg-panel)
                             (make-autobot user-queue
                                (list ghosting user-queue dealer1))))
                         (else
                          (let ([adjusted-time (- adjusted-value 6)])
                               (send-msg 
                                 (string-append "Fighting a robot in "
                                   (string-append (number->string adjusted-time)
                                                  " seconds"))
                                         msg-panel)
                               (make-autobot user-queue
                                 (list time-delay dealer1 
                                       adjusted-time))))))))
	    ;; run threaded window refresher
	    (set-panel-refresh user1 dealer1 set-master-panel msg-panel)
	    ;; run threaded set-counter
            (users-set-counter user-queue
               (gen-horizontal-panel master-msg-panel #f))
            ;; run threaded timer using dealer
            (set-timer dealer1
               (gen-child-msg-panel master-msg-panel
                  "Timer" #f))
            ;; blanket method to toggle notifications
            (if (false? notifications?)
                #f
                ((lambda ()
                   (notify-set-found dealer1 msg-panel)
                   (notify-game-over dealer1 msg-panel))))
	    ;; play the background muzak
	    (if (false? music?)
	        #f
	        (game-music))
	    ;; return the set-master-window
	    set-master-window)))

;; generate a new game
(define (new-game)
  (send (gen-new-game) show #t))
   
;; run some programs
(new-game)
