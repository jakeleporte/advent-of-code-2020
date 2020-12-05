#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim))

(define slopes '((1 . 1) (1 . 3) (1 . 5) (1 . 7) (2 . 1)))

(define (read-grid)
  "Read an array of 'trees' and 'open squares' from
standard input"
  (let loop ((line (read-line))
	     (grid '()))
    (if (eof-object? line)
	  (list->array 2 grid)
	(loop (read-line)
	      (append grid (list (string->list line)))))))

(define (count-trees grid slope)
  ;; Width of the grid
  (define width (array-length (array-cell-ref grid 0)))
  (define di (car slope))
  (define dj (cdr slope))
  (let loop ((i 0) (j 0) (count 0))
    (if (array-in-bounds? grid i j)
	(if (char=? (array-ref grid i j) #\#)
	    (loop (+ i di) (modulo (+ j dj) width) (1+ count))
	    (loop (+ i di) (modulo (+ j dj) width) count))
	count)))

(define (main)
  (let ((grid (read-grid)))
    (let loop ((slope-list slopes)
	       (total 1))
      (if (null? slope-list)
	  (begin (display total) (newline))
	  (loop (cdr slope-list)
		(* total (count-trees grid (car slope-list))))))))

(main)
