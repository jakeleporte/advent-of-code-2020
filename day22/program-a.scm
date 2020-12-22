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

(define (combat deck-1 deck-2)
  "Each round, a card is drawn from DECK-1 and DECK-2; the higher
card wins and is enqueued in the winning deck, then the losing card
is enqueued in the winning deck, and play continues until a deck is
empty."
  (if (not (or (q-empty? deck-1)
	       (q-empty? deck-2)))
      (let* ((card-1 (q-front deck-1))
	     (card-2 (q-front deck-2))
	     (winner (if (> card-1 card-2) deck-1 deck-2))
	     (loser (if (< card-1 card-2) deck-1 deck-2)))
	(enq! winner (deq! winner))
	(enq! winner (deq! loser))
	(combat deck-1 deck-2))
      (deck-score (if (q-empty? deck-1) deck-2 deck-1))))

(define (main)
  (let ((deck-1 (read-deck))
	(deck-2 (read-deck)))
   (display (combat deck-1 deck-2))))

(main)
