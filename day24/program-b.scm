#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 match))

(define adj '((2 . 0) (-2 . 0) (1 . -1)
	      (-1 . -1) (1 . 1) (-1 . 1)))

(define (add-coords c1 c2)
  (cons (+ (car c1) (car c2))
	(+ (cdr c1) (cdr c2))))

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

(define (count-adjacent tiles coord)
  (define count 0)
  (for-each
   (lambda (dc)
     (let ((state (hash-ref tiles (add-coords coord dc) 0)))
       (set! count (+ count state))))
   adj)
  count)

(define (copy-hash-table ht)
  (define new (make-hash-table))
  (hash-for-each
   (lambda (k v)
     (hash-set! new k v))
   ht)
  new)

(define (expand-grid grid)
  (define expanded (copy-hash-table grid))
  (hash-for-each
   (lambda (k v)
     (when (= v 1)
       (for-each
	(lambda (dc)
	  (let* ((coord (add-coords k dc))
		 (state (hash-ref expanded coord)))
	    (when (not state)
	      (hash-set! expanded coord 0))))
	adj)))
   grid)
  expanded)

(define (conway-hex tiles n)
  (let loop ((last (expand-grid tiles)) (next (expand-grid tiles)) (left n))
    (if (= left 0) last
	(begin
	  (hash-for-each
	   (lambda (k v)
	     (let ((count (count-adjacent last k)))
	       (cond ((and (= v 1) (or (= count 0) (> count 2)))
		      (hash-set! next k 0))
		     ((and (= v 0) (= count 2))
		      (hash-set! next k 1)))))
	   last)
	  (loop (expand-grid next) (expand-grid next) (1- left))))))

(define (conway-hex-init)
  (define grid (make-hash-table))
  (let loop ((line (read-line)))
    (if (eof-object? line) grid
	(let* ((coord (directions->coordinates line))
	       (curr (hash-ref grid coord 0)))
	  (hash-set! grid coord (modulo (1+ curr) 2))
	  (loop (read-line))))))

(define (main)
  (let* ((last (conway-hex (conway-hex-init) 100)))
    (display (hash-count (lambda (k v) (= v 1)) last))
    (newline)))

(main)
