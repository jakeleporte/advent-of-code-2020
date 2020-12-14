#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 arrays))

(define (read-grid)
  "Read in an array of characters representing
an initial seat arrangement"
  (let loop ((seats '()) (line (read-line)))
    (if (eof-object? line) (list->array 2 (reverse seats))
	(loop (cons (string->list line) seats) (read-line)))))

(define (count-visible-filled seats x y)
  (define dirs '((-1 . -1) (-1 . 0) (-1 . 1)
		 (0 . -1)           (0 . 1)
		 (1 . -1)  (1 . 0)  (1 . 1)))
  (define count 0)
  (for-each
   (lambda (coor)
     (let loop ((i (+ (car coor) x)) (j (+ (cdr coor) y)))
       (cond ((not (array-in-bounds? seats i j)) #f)
	     ((char=? (array-ref seats i j) #\L) #f)
	     ((char=? (array-ref seats i j) #\#) (set! count (1+ count)))
	     ((char=? (array-ref seats i j) #\.)
	      (loop (+ i (car coor)) (+ j (cdr coor)))))))
   dirs)
  count)

(define (iterate seats)
  (define next-seats (array-copy seats))
  (array-index-map! next-seats
   (lambda (x y)
     (cond ((and (char=? (array-ref seats x y) #\L)
		 (= (count-visible-filled seats x y) 0)) #\#)
	   ((and (char=? (array-ref seats x y) #\#)
		 (> (count-visible-filled seats x y) 4)) #\L)
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
