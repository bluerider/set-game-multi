#lang racket/base

;; GUI library for play-set

;; provide the following modules
(provide set-panel-refresh)
(provide get-hint)
(provide set-timer)
(provide user-set-counter)
(provide send-msg)
(provide make-button)
(provide notify-set-found)
(provide view-sets)
(provide view-sets-panel-refresh)
(provide gen-horizontal-panel)
(provide gen-set-panel)
(provide gen-child-msg-panel)
(provide make-button)
(provide gen-robot-slider)
(provide notify-game-over)
(provide gen-set-frame)
(provide users-set-counter)

;; load required modules
(require "image-library.rkt")
(require "user-library.rkt")
(require "game-library.rkt")
(require "dealer-library.rkt")
(require "sound-library.rkt")
(require data/queue)
(require racket/gui/base)
(require racket/class)
(require racket/bool)
(require racket/list)

;; generate new card-button class derived from button class
(define card-button%
  ;; inherit button class
  (class button%
     ;; initialize the card field
     (field (card '(6 6 6 6)))
     ;; create the class
     (super-new))
  )

;; make a button in the button-panel
(define (make-button button-panel title proc width?)
  (new card-button% [parent button-panel]
                    [label title]
                    [stretchable-width width?]
                    [callback (lambda (button event)
				      (proc button))]))
;; make a card in the set-panel
(define (make-card set-panel bitmap proc)
  (new card-button% [parent set-panel]
                    [label bitmap]
                    [stretchable-width #t]
                    [stretchable-height #t]
                    [vert-margin 0]
                    [horiz-margin 0]
                    [callback (lambda (button event)
                                      (proc button))]))

;; send a message to the msg-panel
(define (send-msg msg msg-panel)
  (send msg-panel set-label msg))
                                      
(define (get-hint dealer msg-panel)
  (let ([found-sets (length (find-all-sets (deal-cards dealer)))])
       (send-msg (string-append (number->string found-sets)
                                " sets available")
		 msg-panel)))

;; procedure upon card button click in window
(define (user-click button user dealer msg-panel)
            ;; get the selected-card
   (letrec ([selected-card (get-field card button)]
            ;; get the user-hand (list)
            [user-hand (show-hand user)]
            ;; check if card is duplicated
            [duplicate-card (member selected-card user-hand)])
           (if (false? duplicate-card)
               ;; add the card to the user's hand
	       (add-card user selected-card)
               ;; remove card from the user's hand
	       (remove-card user selected-card))
           ;; notify the user of how many cards selected
           (send-msg (string-append "Selected "
                        (string-append (number->string (hand-size user))
                            (if (= 1 (hand-size user))
                                     " card"
                                     " cards")))
		     msg-panel)
           ;; submit cards to be checked if selected at least 3 cards
	   (if (> 3 (hand-size user))
               #f
               (check-hand user dealer))))

;; split the list of cards into lists for columns
(define (split-matrix-gen card-pool)
  ;; get the integer square root of the length of card-pool
  (define gsf (integer-sqrt (length card-pool)))
  ;; generate square-like array of array of cards
  (define (iterator lat)
    (if (> gsf (length lat))
        (list lat)
        (cons (take lat gsf)
              (iterator (drop lat gsf)))))
  (iterator card-pool))
	  
;; draw the cards on the set-panel
;;   need a set-matrix
;;   needs a user (user doesn't have to do anything though)
;;   needs a dealer
(define (cards->columns set-matrix user dealer set-master-panel old-set-panel msg-panel)
  (let ([split-matrix (split-matrix-gen set-matrix)]
	[set-panel (new horizontal-panel% [parent set-master-panel]
					  [stretchable-height #t]
					  [stretchable-width #t])])
      ;; set up the cards
      (define (set-up-cards scale)
        (for-each (lambda (card-clump)
                    ;; generate a column
                    (let ([column (new vertical-panel% [parent set-panel])])
                         (for-each (lambda (new-card)
                                     ;; generate the card button
                                     (let ([card-button 
                                             (make-card column
                                             ;; get image for card with scale
                                             (number->image new-card scale)
                                               (lambda (button)
                                                  (user-click button user 
                                                     dealer msg-panel)))])
                                          ;; enter the card's value into the card field
                                          (set-field! card card-button new-card)))
			               card-clump)))
                split-matrix))
      ;; check if an old set panel was passed
      (if (null? old-set-panel)
	  #f
	  (begin
            ;; disable and delete old set panel
            (send old-set-panel enable #f)
            (send set-master-panel delete-child old-set-panel)))
      ;; set up cards
      (set-up-cards (get-backing-size split-matrix set-master-panel))
      ;; return the set-panel
      set-panel))
      
;; get the backing size for cards in a panel
(define (get-backing-size split-matrix set-master-panel)
  ;; get the greatest scaling factor
  (car (sort (map (lambda (panel-size bitmap-size dimension)
                    ;; get the scaling factor for a dimension
                    (/ (* bitmap-size dimension) 
                       panel-size))
                  ;; get panel size
                  (list (send set-master-panel get-width)
                        (send set-master-panel get-height))
                  ;; card bitmap 96x52 (wxh)
                  '(96 52)
                  ;; get dimensions of split-matrix\
                  (list (length split-matrix)
                        (length (car split-matrix))))
             >)))
                
;; refresh the set-panel every 250 milliseconds
(define (set-panel-refresh user dealer set-master-panel msg-panel)
  ;; get size of panel in (width height)
  (define (get-panel-size panel)
    (list (send panel get-width)
          (send panel get-height)))
  ;; refresh panel thread
  (define (threaded-panel-refresh old-hand old-panel old-size)
     (let ([new-hand (deal-cards dealer)])
          ;; sleep for 250 milliseconds
          (sleep .250)
          ;; check if set-panel needs updating
          ;;   don't refresh panel if it's not shown
          (if (or (false? (send set-master-panel is-shown?))
                  (zero? (length (show-hand dealer)))
                  (and (equal? old-hand new-hand)
                       (equal? old-size (get-panel-size set-master-panel))))
              (threaded-panel-refresh old-hand old-panel 
                 (get-panel-size set-master-panel))
              (threaded-panel-refresh new-hand 
                 (cards->columns new-hand user dealer set-master-panel 
                                 old-panel msg-panel)
                 ;; get the new size of the panel
                 (get-panel-size set-master-panel)))))
  ;; launch refresh panel thread
  (thread (lambda ()
             ;; yield the thread
             (sleep 0)
             ;; run the set-panel-refresh instance
             (threaded-panel-refresh (deal-cards dealer) 
                (cards->columns (deal-cards dealer) 
                                user dealer set-master-panel 
                                '() 
                                msg-panel)
                ;; get the current window size
                (get-panel-size set-master-panel)))))
                
;; insert a timer in the time-panel
(define (set-timer user time-panel)
  (letrec ([threaded-timer (lambda ()
                             (letrec ([user-start (start-time user)]
                                      [current-time (current-milliseconds)]
                                      [real-time (/ (- current-time user-start) 
                                                    1000)]
                                      [real-time-minutes (floor (/ real-time 60))]
                                      [real-time-seconds 
                                         (round (- real-time 
                                                   (* real-time-minutes 60)))])
                                     ;; update time panel with current passed time
                                     (send time-panel set-label 
                                        (string-append 
                                           (number->string real-time-minutes)
                                              (string-append ":"
                                                (if (> 10 real-time-seconds)
                                                    (string-append "0" 
                                                       (number->string 
                                                          real-time-seconds))
                                                    (number->string 
                                                       real-time-seconds))))))
				   ;; sleep for 1 second
				   (sleep 1)
				   ;; recurse the set-timer
				   (threaded-timer))])
          (thread (lambda ()
                          ;; yield the thread
                          (sleep 0)
                          ;; insert a timer in the time-panel
                          (threaded-timer)))))
          
;; insert a user set counter in the time-panel
(define (user-set-counter user user-sets-panel)
  (letrec ([threaded-counter  (lambda (old-size)
                                   (let ([current-size (sets-size user)])
                                        (if (= old-size current-size)
					    #f
                                            ;; update the user-sets-panel with the # found sets
                                            (send user-sets-panel set-label 
;;                                               (string-append "Sets Found: "
                                              (string-append ""
                                                 (string-append (number->string current-size)
;;                                                                 "  |"))))
                                                                " |"))))
					;; sleep for 0.25 second
				        (sleep 0.25)
                                        ;;recurse the user-set-counter
                                        (threaded-counter current-size)))])
       (thread (lambda ()
		       ;; yield the thread
                       (sleep 0)
                       ;; insert a user-set counter in the user-sets-panel
                       (threaded-counter -1)))))

;; use user-set-counter for a user queue
(define (users-set-counter user-queue msg-panel)
  (define (threaded-notify prev-users)
             ;; get new users
    (let ([new-users (remove* prev-users (queue->list user-queue))])
         (if (null? new-users)
             #f
             ;; generate a counter for each new user
             (for-each 
               (lambda (user)
                 (user-set-counter user (gen-child-msg-panel msg-panel 
                                           "User Sets: " #f)))
               ;; get only new users
               new-users))
          ;; sleep for 250 ms
          (sleep .250)
          ;; recurse and pass current new users
          (threaded-notify (append new-users prev-users))))
  (thread (lambda ()
                  ;; yield the thread
                  (sleep 0)
                  ;; look for changes in the user-queue
                  (threaded-notify '()))))
                  
;; insert a counter to notify the user whenever they found a set
(define (notify-set-found user msg-panel)
  (letrec ([threaded-notify (lambda (old-size)
                                    (let ([current-size (sets-size user)])
				         (if (= old-size current-size)
				             #f
				             ;; notify the user that a new set was found
				             (begin
				              (send-msg "Found set!" msg-panel)
				              (found-set-sound)))
					 ;; sleep for 250 ms
					 (sleep .250)
				         (threaded-notify current-size)))])
	  (thread (lambda ()
	                  ;; yield the thread
	                  (sleep 0)
	                  (threaded-notify (sets-size user))))))
	                  
;; check if the game is over
(define (notify-game-over dealer msg-panel)
  (define (threaded-notify)
     ;; check if the dealer has any sets left
     ;;    this can be an expensive operation
     ;;    check if the dealer has less than 15 cards left
     (if (and (>= 15 (length (show-hand dealer)))
              (null? (find-all-sets (show-hand dealer))))
         (begin
            (send-msg "Game Over!" msg-panel)
            (game-over-sound)
            (kill-thread (current-thread)))
         #f)
     ;; sleep for 250 ms
     (sleep .250)
     (threaded-notify))
  (thread (lambda ()
            ;; yield the thread
            (sleep 0)
            (threaded-notify))))
	                  
;; generate a floating window of all found-sets
(define (view-sets user set-master-panel old-set-panel)
  (letrec ([found-sets (if (null? (get-sets user))
                           '()
                           (split-matrix-gen (flatten (get-sets user))))]
           [set-panel (new vertical-panel% [parent set-master-panel]
                                           [stretchable-height #t]
                                           [stretchable-width #t])])
	  ;; check if an old set panel was passed
      (if (null? old-set-panel)
	  #f
	  (begin
            ;; disable and delete old set panel
            (send old-set-panel enable #f)
            (send set-master-panel delete-child old-set-panel)))
      ;; set up the cards
      (for-each (lambda (card-clump)
                              ;; generate a column
			(let ([row (new horizontal-panel% [parent set-panel])])
			     (for-each (lambda (new-card)
			                 ;; add card
                                         (new card-button% [parent row]
                                                           [label (number->image new-card 2.0)]
                                                           [stretchable-width #t]
                                                           [stretchable-height #t]))
			               card-clump)))
                found-sets)
      ;; return the set-panel
      set-panel))

;; continuously show and update all found user sets
;; (define (view-sets-panel-refresh user label)
(define view-sets-panel-refresh
  (lambda (user
           label
           ;; optionally set the set-master-panel
           #:set-master-panel [set-master-panel
                                (new frame% [label label]
                                            [alignment '(center center)])])
     ;; panel refresh thread
     (define (threaded-panel-refresh old-hand old-panel)
        (let ([new-hand (flatten (get-sets user))])
             ;; sleep for 250 milliseconds
             (sleep .250)
             ;; check if set-panel needs updating
             ;;   don't update if panel's not shown
             (if (or (false? (send set-master-panel is-shown?))
                     (equal? old-hand new-hand))
                 (threaded-panel-refresh old-hand old-panel)
                 (threaded-panel-refresh new-hand
                    (view-sets user 
                              set-master-panel 
                              old-panel)))))
     (thread (lambda ()
                ;; yield the thread
                (sleep 0)
                ;; run the set-panel-refresh instance
                (threaded-panel-refresh (flatten (get-sets user)) 
                                        (view-sets user set-master-panel '()))))
     ;; show the set-master-panel
     (send set-master-panel show #t)
     ;; return the set-master-panel object
     set-master-panel))
			                          
;; easy horizontal panel generation
(define (gen-horizontal-panel frame width?)
   (new horizontal-panel% [parent frame]
                          [stretchable-width width?]
                          [stretchable-height #f]))
;; generate set panel
(define (gen-set-panel frame)
   (new horizontal-panel% [parent frame]
                          [stretchable-width #t]
                          [stretchable-height #t]
                          [min-width 455]
                          [min-height 240]))
                          
;; generate message panels
(define (gen-child-msg-panel msg-panel name width?)
   (new message% [parent msg-panel]
                 [label name]
                 [auto-resize #t]
                 [stretchable-width width?]))
                 
(define (gen-set-frame label fullscreen?)
  (if (false? fullscreen?)
      (new frame% [label label]
                  [alignment '(center center)])
      (new frame% [label label]
                  [alignment '(center center)]
                  [width (call-with-values
                           (lambda ()
                             (get-display-size))
                           (lambda (width height)
                             width))]
                  [height (call-with-values
                            (lambda ()
                              (get-display-size))
                            (lambda (width height)
                              height))]
                  [style (list 'no-caption)])))
                 
;; generate the Robot slider
(define (gen-robot-slider panel robot-proc)
   (new slider% [parent (new vertical-panel% [parent panel]
                                             [stretchable-height #t]
                                             [stretchable-width #f]
                                             [vert-margin 0]
                                             [horiz-margin 0])]
                [label "Robot"]
                [min-value 0]
                [max-value 12]
                [style (list 'vertical-label 'vertical)]
                [stretchable-width #t]
                [callback (lambda (slider event)
                             (robot-proc (send slider get-value)))]))
