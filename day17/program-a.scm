#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 arrays))

(define (read-seed)
  (let loop ((line (read-line))
	     (grid '()))
    (if (eof-object? line)
	(list->array 2 grid)
	(loop (read-line) (cons (string->list line) grid)))))

(define (count-neighbors arr idx idy idz)
  "Count active neighbors of a cell in arr"
  (define neighbors '((-1 -1 -1) (-1 -1 0) (-1 -1 1)
		      (-1  0 -1) (-1  0 0) (-1  0 1)
		      (-1  1 -1) (-1  1 0) (-1  1 1)
		      (0  -1 -1) (0  -1 0) (0  -1 1)
		      (0   0 -1)           (0   0 1)
		      (0   1 -1) (0   1 0) (0   1 1)
		      (1  -1 -1) (1  -1 0) (1  -1 1)
		      (1   0 -1) (1   0 0) (1   0 1)
		      (1   1 -1) (1   1 0) (1   1 1)))
  (define count 0)
  (for-each
   (lambda
       (dp)
     (let ((x (+ idx (car dp)))
	   (y (+ idy (cadr dp)))
	   (z (+ idz (caddr dp))))
       (and (array-in-bounds? arr x y z)
	    (char=? (array-ref arr x y z) #\#)
	    (set! count (1+ count)))))
   neighbors)
  count)

(define (make-conway-grid seed cycles)
  (define grid (make-array #\.
			   (+ (array-length seed) (* 2 cycles))
			   (+ (array-length seed) (* 2 cycles))
			   (1+ (* 2 cycles))))
  (let ((inner-grid (make-shared-array
		     grid
		     (lambda (x y) (list (+ x cycles)
					 (+ y cycles)
					 cycles))
		     (array-length seed) (array-length seed))))
    (array-copy! seed inner-grid))
  grid)

(define (conway-it grid)
  (define old (array-copy grid))
  (array-index-map!
   grid
   (lambda (x y z)
     (let ((count (count-neighbors old x y z))
	   (state (array-ref old x y z)))
       (if (char=? state #\#)
	   (if (or (= count 2) (= count 3))
	       #\# #\.)
	   (if (= count 3) #\# #\.)))))
  grid)

(define (conway-life seed cycles)
  (let loop ((grid (make-conway-grid seed cycles)) (i cycles))
    (if (= i 0) grid (loop (conway-it grid) (1- i)))))

(define (count-active grid)
  (define count 0)
  (array-for-each
   (lambda (e) (when (char=? e #\#)
		 (set! count (1+ count))))
   grid)
  count)

(define (main)
  (display (count-active (conway-life (read-seed) 6)))
  (newline))

(main)
