#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (srfi srfi-1))

(define (earliest-bus)
  "Return the wait time and bus-id of the earliest leaving bus."
  (let* ((arrival-time (string->number (read-line)))
	 (busses (map string->number
		      (string-tokenize (read-line) char-set:digit)))
	 (wait-times (map (lambda (bus) (- bus (modulo arrival-time bus)))
			  busses))
	 (target (reduce (lambda (e1 e2) (if (< (car e1) (car e2)) e1 e2))
			 #f
			 (zip wait-times busses))))
    (values (car target) (cadr target))))

(define (main)
  (display (call-with-values earliest-bus *)) (newline))

(main)
