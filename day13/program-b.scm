#!/usr/bin/env guile
!#

;;; Implements the seive algorithm from
;;; https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Search_by_sieving

(use-modules (ice-9 rdelim) (srfi srfi-1))

(define (read-input)
  "Return vector of list of (NUMBER . MODULUS), sorted by
decreasing modulus"
  (read-line)
  (let ((bus-ids (map string->number
		      (string-tokenize (read-line) char-set:letter+digit))))
    (sort (map (lambda (e) (cons (modulo (car e) (cadr e)) (cadr e)))
	       (filter (lambda (e) (cadr e))
		       (zip (iota (length bus-ids) 0 -1) bus-ids)))
	  (lambda (e1 e2) (> (cdr e1) (cdr e2))))))

(define (crt-seive congruences)
  "Solves the input system of congruences using the
Chinese Remainder Theorem-based Sieve Search method"
  (let loop ((next (car (cadr congruences)))
	     (mod (cdr (cadr congruences)))
	     (step (cdr (car congruences)))
	     (remaining (cdr (cdr congruences)))
	     (result (car (car congruences))))
    (cond ((and (null? remaining)
		(= (modulo result mod) next))
	   result)
	  ((= (modulo result mod) next)
	   (loop (car (car remaining))
		 (cdr (car remaining))
		 (* step mod)
		 (cdr remaining)
		 result))
	  (else
	   (loop next mod step remaining (+ result step))))))

(define (main)
  (display (crt-seive (read-input)))
  (newline))

(main)
