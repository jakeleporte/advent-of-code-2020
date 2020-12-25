#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim))

(define* (public-key->loop-size public-key #:optional (base 7) (mod 20201227))
  "Compute the discrete logarithm using trial multiplication"
  (let loop ((loop-size 1))
    (if (= (modulo-expt base loop-size mod) public-key)
	loop-size
	(loop (1+ loop-size)))))

(define (main)
  (let ((loop-size-1 (public-key->loop-size (string->number (read-line)))))
    (display (modulo-expt (string->number (read-line)) loop-size-1 20201227))
    (newline)))

(main)
