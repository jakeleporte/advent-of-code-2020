#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 q) (srfi srfi-1))

(define (read-deck)
  "Read in a deck of cards from stdin as a queue"
  ;; Discard deck header
  (read-line)
  (define deck (make-q))
  (let loop ((line (read-line)))
    (if (or (eof-object? line) (string-null? line))
	deck
	(begin
	  (enq! deck (string->number line))
	  (loop (read-line))))))

(define (deck-score deck)
  "Multiply each card value in DECK by its position in the
queue, with the rear being worth 1, and the front being worth
(length DECK)"
  (fold (lambda (card value prev)
	  (+ (* card value) prev))
	0
	(car deck)
	(iota (length (car deck))
	      (length (car deck))
	      -1)))

(define (hash-round deck-1 deck-2)
  (let ((s1 (string-join (map number->string (car deck-1))))
	(s2 (string-join (map number->string (car deck-2)))))
    (string-append s1 s2)))
    

(define* (copy-deck deck #:optional card)
  (let ((new-deck (copy-tree deck)))
    (if card
	(begin
	  (set-car! new-deck (take (car new-deck) card))
	  (sync-q! new-deck)
	  new-deck)
	new-deck)))

(define (combat-round deck-1 deck-2 seen break)
  (if (hash-ref seen (hash-round deck-1 deck-2))
      (break 1)
      (begin
	(hash-set! seen (hash-round deck-1 deck-2) #t)
	(let* ((card-1 (deq! deck-1))
	       (card-2 (deq! deck-2))
	       (len-1 (q-length deck-1))
	       (len-2 (q-length deck-2)))
	  (if (or (< len-1 card-1) (< len-2 card-2))
	      (if (> card-1 card-2) 1 2)
	      (combat-rec (copy-deck deck-1 card-1)
			  (copy-deck deck-2 card-2)))))))

(define (combat-rec deck-1 deck-2)
  (define seen (make-hash-table))
  (call/cc
   (lambda (break)
     (begin
       (while (not (or (q-empty? deck-1)
		       (q-empty? deck-2)))
	 (let ((card-1 (q-front deck-1))
	       (card-2 (q-front deck-2))
	       (winner (combat-round deck-1 deck-2 seen break)))
	   (if (= winner 1)
	       (begin
		 (enq! deck-1 card-1)
		 (enq! deck-1 card-2))
	       (begin
		 (enq! deck-2 card-2)
		 (enq! deck-2 card-1)))))
       (if (q-empty? deck-1) 2 1)))))

(define (main)
  (let* ((deck-1 (read-deck))
	 (deck-2 (read-deck))
	 (winner (if (= (combat-rec deck-1 deck-2) 1)
		     deck-1 deck-2)))
    (display (deck-score winner)) (newline)))

(main)
