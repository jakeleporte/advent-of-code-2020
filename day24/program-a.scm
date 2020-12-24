#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 match))

(define (directions->coordinates dirs)
  "Map the hexagonal tiling onto a cartesian 2-d grid.
Choose e-w orientation (two tiles separated from each
other by one other tile in the e-w direction are adjacent)"
  (let loop ((rest dirs) (x 0) (y 0))
    (if (string-null? rest) (cons x y)
	(let ((first (string-ref rest 0)))
	  (match first
	    (#\e (loop (substring rest 1) (+ x 2) y))
	    (#\w (loop (substring rest 1) (- x 2) y))
	    (_ (let ((second (string-ref rest 1)))
		 (match (string first second)
		   ("se" (loop (substring rest 2) (+ x 1) (- y 1)))
		   ("sw" (loop (substring rest 2) (- x 1) (- y 1)))
		   ("ne" (loop (substring rest 2) (+ x 1) (+ y 1)))
		   ("nw" (loop (substring rest 2) (- x 1) (+ y 1)))))))))))

(define (flip-tiles)
  (define flipped (make-hash-table))
  (let loop ((line (read-line)))
    (if (eof-object? line) flipped
	(let ((coord (directions->coordinates line)))
	  (hash-set! flipped coord (not (hash-ref flipped coord)))
	  (loop (read-line))))))

(define (main)
  (let ((tiles (flip-tiles)))
    (display (hash-count (lambda (k v) v) tiles))
    (newline)))

(main)
