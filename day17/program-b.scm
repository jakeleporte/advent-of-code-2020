#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (srfi srfi-1) (ice-9 arrays))

(define (read-seed)
  (let loop ((line (read-line))
	     (grid '()))
    (if (eof-object? line)
	(list->array 2 grid)
	(loop (read-line) (cons (string->list line) grid)))))

(define (count-neighbors arr idx idy idz idw)
  "Count active neighbors of a cell in arr"
  (define relative '(-1 0 1))
  (define count 0)
  (for-each
   (lambda (dx)
     (for-each
      (lambda (dy)
	(for-each
	 (lambda (dz)
	   (for-each
	    (lambda (dw)
	      (if (not (equal? (list dx dy dz dw) '(0 0 0 0)))
		  (let ((x (+ idx dx)) (y (+ idy dy))
			(z (+ idz dz)) (w (+ idw dw)))
		    (when (and (array-in-bounds? arr x y z w)
			       (char=? (array-ref arr x y z w) #\#))
		      (set! count (1+ count))))))
	    relative))
	 relative))
      relative))
   relative)
  count)

(define (make-conway-grid seed cycles)
  (define grid (make-array #\.
			   (+ (array-length seed) (* 2 cycles))
			   (+ (array-length seed) (* 2 cycles))
			   (1+ (* 2 cycles))
			   (1+ (* 2 cycles))))
  (let ((inner-grid (make-shared-array
		     grid
		     (lambda (x y) (list (+ x cycles)
					 (+ y cycles)
					 cycles
					 cycles))
		     (array-length seed) (array-length seed))))
    (array-copy! seed inner-grid))
  grid)

(define (conway-it grid)
  (define old (array-copy grid))
  (array-index-map!
   grid
   (lambda (x y z w)
     (let ((count (count-neighbors old x y z w))
	   (state (array-ref old x y z w)))
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
