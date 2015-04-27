#lang racket

(require "radix-library.rkt")

(provide factorial)
(provide combinatorial)
(provide average)
(provide set-deck)
(provide gen-unique-seeds)
(provide draw-card)
(provide gen-unique-matrix)
(provide shuffle-deck)
(provide random-set)
(provide unique-set?)
(provide set?)
(provide boolean-set?)
(provide find-set)
(provide find-all-sets-monte-carlo)
(provide find-all-sets)
(provide p-find-all-sets)
(provide find-all-sets-no-conflicts)
(provide gen-set-game)
(provide solutions->set-matrix)
(provide unique?)
(provide only-unique-sets)

;; n!p = n*(n-1)*(n-2)*...*(n-p+1)
(define (factorial n p)
   (for/product ([i (range (add1 (- n p)) (add1 n) 1)])
      i))
   
;; nCp = (nPp)/p!
(define (combinatorial n p)
   (/ (factorial n p)
      (factorial p p)))
   
;; find the average of a list
(define (average list-of-numbers)
   (/ (for/sum ([ i list-of-numbers]) i)
      (length list-of-numbers)))
  
;; generate unique random seeds
(define (gen-unique-seeds n max)
  (define (iterator lat count)
    (if (zero? count)
        '()
        (let ([random-seed (list-ref lat (random (length lat)))])
             (cons random-seed
               (iterator (remove random-seed lat)
                         (sub1 count))))))
  (iterator (range (add1 max)) n))
  
;; draw a particular card from the set matrix
(define (draw-card num set-matrix)
   (list-ref set-matrix num))
  
;; generate (size) unique random seeds
(define (gen-unique-matrix size)
  (gen-unique-seeds size 80))

;; generate the set deck
(define (set-deck)
  (range 81))
   
;; shuffle the set deck
(define (shuffle-deck)
  (gen-unique-matrix 81))
   
;; randomly draw three cards to form a set from the set-matrix
(define (random-set set-matrix)
  (map (lambda (x) (draw-card x set-matrix))
       (gen-unique-seeds 3 (sub1 (length set-matrix)))))

;; check if a set is unique among found sets
(define (unique-set? set found-sets)
   (andmap (lambda (known-set)
                   (not (and (member (car set) known-set)
                             (member (cadr set) known-set)
                             (member (caddr set) known-set))))
           found-sets)
   )
   
;; return only a list of all unique sets
(define (only-unique-sets set)
   (if (null? (flatten set))
       '()
       (foldl (lambda (pos-set condensor)
                      (if (unique-set? pos-set condensor)
                          (cons pos-set condensor)
                          condensor))
              (list (car set))
              (cdr set))))  
        
;; verify if chosen three cards constitute a set
;;    for each property: all the same or all different
;; passed set is a set of 3 set cards
;;   * andmap function returns false for the first false
(define (set? set)
   (cond ((not (list? set)) #f)
         ((and (= 3 (length set)) (unique? set))
          (let ([cards (map integer->card set)])
               (andmap (lambda (card1 card2 card3)
                         (zero? (remainder (+ card1 card2 card3)
                                           3)))
                       (car cards)
                       (cadr cards)
                       (caddr cards))))
	 (else #f)))
	 
(define (boolean-set? set)
  (cond ((not (list? set)) #f)
        ((and (= 3 (length set)) (unique? set))
         (let ([cards (map integer->card set)])
              (andmap (lambda (card1 card2 card3)
                        (eq? (equal? card1 card2)
                             (equal? card2 card3))) 
                      (car cards)
                      (cadr cards)
                      (caddr cards))))
        (else #f)))
        
;;find a set in the set-matrix using a monte-carlo method
(define (find-set set-matrix)
   (letrec ([iterator (lambda (count)
                         (let ([set (random-set set-matrix)])
                               (cond ((zero? count) #f)
                                     ((set? set) set)
                                     (else
                                      (iterator (sub1 count))))))])
           (if (null? set-matrix)
               #f
               (iterator (combinatorial (length set-matrix)
                                        3)))))
   
;; find all sets in the set-matrix
(define (find-all-sets-monte-carlo set-matrix)
   (letrec ([iterator (lambda (count found-sets)
                         (let ([pos-set (find-set set-matrix)])
                              (cond ((false? pos-set) '())
                                    ((<= count 0) found-sets)
                                    (else
                                     (iterator (sub1 count)
                                               (if (unique-set? pos-set
                                                                found-sets)
                                                   (cons pos-set
                                                         found-sets)
                                                   found-sets))))))])
           (iterator (factorial (length set-matrix) 3) '())))

;; find all sets deductively
(define (find-all-sets set-matrix)
   (define (iterator count)
      (if (<= count 1)
          '(())
          (for*/list ([y (iterator (sub1 count))]
                      [x set-matrix])
                   (cons x y))))
   (only-unique-sets (filter set? (iterator 4))))
   
;; find all sets deductively using places
(define (p-find-all-sets set-matrix)
   (define all-pos-sets
      (letrec ([iterator (lambda (count)
                            (if (<= count 1)
                                '(())
                                (for*/list ([y (iterator (sub1 count))]
                                            [x set-matrix])
                                           (cons x y))))])
         (only-unique-sets (filter set? (iterator 4)))))
   (define chunked-lat
       (letrec ([chunk-count (ceiling (/ (length all-pos-sets) (processor-count)))]
                [iterator (lambda (chunks rest)
                             (if (<= (length rest) chunk-count)
                                 (cons rest chunks)
                                 (iterator (cons (take rest chunk-count) chunks)
                                           (drop rest chunk-count))))])
               (iterator '() all-pos-sets)))
   (define place-threads
       (build-list (length chunked-lat)
                   (lambda (x)
                      (place pch
                         (place-channel-put pch (filter set? (place-channel-get pch)))))))
   (for-each place-channel-put place-threads chunked-lat)
   (only-unique-sets (append-map place-channel-get place-threads)))

;; find all non-conflicting sets
(define (find-all-sets-no-conflicts set-matrix)
  (let ([found-sets-matrix (remove-duplicates 
                             (append-map values 
                                         (find-all-sets set-matrix)))])
         (argmax (lambda (condensed-matrix)
                     (length (find-all-sets condensed-matrix)))
                 (map (lambda (card)
                        (remove card found-sets-matrix))
                      found-sets-matrix))))
  
  
;; convert the solution set into a normal sets
(define (solutions->set-matrix found-sets)
   (foldl (lambda (cards card-box)
                  (append cards card-box))
                  '()
                  found-sets)
   )

;; generate n-size game with at least num_solutions
(define (gen-set-game size num_solutions)
         ;; max iterations = 80!12
   (let ([max_iter (combinatorial 80 size)])
                ;; iterator
        (define (gen-set-game-iter num)
               ;; check if # max iterations is exceeded
           (if (> num max_iter)
               ;; print #f if no matrix works
               #f
                     ;; generate unique 12 element matrix
               (let ([set-matrix (gen-unique-matrix size)])
                        ;; check if at least 6 sets are found
                    (if (<= num_solutions (length (find-all-sets set-matrix)))
                        ;; print the set-matrix
                        set-matrix
                        ;; else iterate
                        (gen-set-game-iter (+ num 1)))))
           )
        ;; run iterator
        (gen-set-game-iter 1))
   )
   
;; check if elements are unique
(define (unique? lat)
	 ;; check if entered list
  (cond ((not (list? lat)) #f)
        (else
           (andmap (lambda (element)
                           ;; check if element is found more than once
                           (not (member element (remove element lat))))
                   lat))))