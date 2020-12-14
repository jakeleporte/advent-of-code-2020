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
  (let loop ((x 0) (y 0) (wx 10) (wy 1))
    (receive (inst arg) (read-inst)
      (if (not inst) (+ (abs x) (abs y))
	  (match inst
	    (#\N (loop x y wx (+ wy arg)))
	    (#\S (loop x y wx (- wy arg)))
	    (#\E (loop x y (+ wx arg) wy))
	    (#\W (loop x y (- wx arg) wy))
	    (#\L (match arg
		   (0 (loop x y wx wy))
		   (90 (loop x y (- wy) wx))
		   (180 (loop x y (- wx) (- wy)))
		   (270 (loop x y wy (- wx)))))
	    (#\R (match arg
		   (0 (loop x y wx wy))
		   (90 (loop x y wy (- wx)))
		   (180 (loop x y (- wx) (- wy)))
		   (270 (loop x y (- wy) wx))))
	    (#\F (loop (+ x (* arg wx)) (+ y (* arg wy)) wx wy)))))))

(define (main)
  (display (execute-instructions)) (newline))

(main)
