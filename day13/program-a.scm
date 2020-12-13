#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (srfi srfi-1))

(define (earliest-bus)
  "Return the input initial arrival time and the wait time
until the earliest bus."
  (let* ((arrival-time (string->number (read-line)))
	 (busses (map string->number
		      (string-tokenize (read-line) char-set:digit)))
	 (wait-times (map (lambda (bus) (- bus (modulo arrival-time bus)))
			  busses))
	 (min-wait (apply min wait-times))
	 (bus-id (find (lambda (bus) (= (- bus (modulo arrival-time bus))
					min-wait)) busses)))
    (values bus-id min-wait)))

(define (main)
  (display (call-with-values earliest-bus *)) (newline))

(main)
