#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim))

(define (read-grid)
  "Read an array of 'trees' and 'open squares' from
standard input"
  (let loop ((line (read-line))
	     (grid '()))
    (if (eof-object? line)
	  (list->array 2 grid)
	(loop (read-line)
	      (append grid (list (string->list line)))))))

(define (count-trees grid)
  ;; Width of the grid
  (define width (array-length (array-cell-ref grid 0)))
  (let loop ((i 0) (j 0) (count 0))
    (if (array-in-bounds? grid i j)
	(if (char=? (array-ref grid i j) #\#)
	    (loop (+ i 1) (modulo (+ j 3) width) (1+ count))
	    (loop (+ i 1) (modulo (+ j 3) width) count))
	count)))

(define (main)
  (let ((grid (read-grid)))
    (display (count-trees grid))))

(main)
