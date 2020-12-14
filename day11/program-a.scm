#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 arrays))

(define (read-grid)
  "Read in an array of characters representing
an initial seat arrangement"
  (let loop ((seats '()) (line (read-line)))
    (if (eof-object? line) (list->array 2 (reverse seats))
	(loop (cons (string->list line) seats) (read-line)))))

(define (count-adjacent-filled seats x y)
  (define adjacent '((-1 . -1) (-1 . 0) (-1 . 1)
		     (0 . -1) (0 . 0) (0 . 1)
		     (1 . -1) (1 . 0) (1 . 1)))
  (define count 0)
  (for-each
   (lambda (coor)
     (when
	 (and (array-in-bounds? seats (+ x (car coor)) (+ y (cdr coor)))
	      (char=? (array-ref seats (+ x (car coor)) (+ y (cdr coor))) #\#))
       (set! count (1+ count)))) adjacent)
  count)

(define (iterate seats)
  (define next-seats (array-copy seats))
  (array-index-map! next-seats
   (lambda (x y)
     (cond ((and (char=? (array-ref seats x y) #\L)
		 (= (count-adjacent-filled seats x y) 0)) #\#)
	   ((and (char=? (array-ref seats x y) #\#)
		 (> (count-adjacent-filled seats x y) 4)) #\L)
	   (else (array-ref seats x y)))))
  next-seats)

(define (count-filled seats)
  (define count 0)
  (array-for-each
   (lambda (e)
     (when (char=? e #\#)
       (set! count (1+ count))))
   seats)
  count)

(define (main)
  (define first-seats (read-grid))
  (let loop ((seats first-seats) (next (iterate first-seats)))
    (if (array-equal? seats next)
	(begin (display (count-filled seats)) (newline))
	(loop next (iterate next)))))

(main)
