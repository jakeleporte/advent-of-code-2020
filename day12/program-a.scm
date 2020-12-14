#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 match) (ice-9 receive))

(define (read-inst)
  (define line (read-line))
  (if (eof-object? line) (values #f #f)
      (let ((inst (string-ref line 0))
	    (arg (string->number (substring line 1))))
	(values inst arg))))

(define (execute-instructions)
  (let loop ((x 0) (y 0) (dir 0))
    (receive (inst arg) (read-inst)
      (if (not inst) (+ (abs x) (abs y))
	  (match inst
	    (#\N (loop x (+ y arg) dir))
	    (#\S (loop x (- y arg) dir))
	    (#\E (loop (+ x arg) y dir))
	    (#\W (loop (- x arg) y dir))
	    (#\L (loop x y (modulo (+ dir arg) 360)))
	    (#\R (loop x y (modulo (- dir arg) 360)))
	    (#\F (match dir
		   (0 (loop (+ x arg) y dir))
		   (90 (loop x (+ y arg) dir))
		   (180 (loop (- x arg) y dir))
		   (270 (loop x (- y arg) dir)))))))))

(define (main)
  (display (execute-instructions)) (newline))

(main)
