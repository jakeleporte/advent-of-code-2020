#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 match))

(define (read-seat-id)
  (let ((line (read-line)))
    (if (eof-object? line) #f
	(string->number
	 (string-map
	  (match-lambda (#\F #\0) (#\B #\1) (#\L #\0) (#\R #\1))
	  line) 2))))

(define (main)
  (let loop ((seat-id (read-seat-id)) (max-id -1))
    (if seat-id (loop (read-seat-id) (max seat-id max-id))
	(begin (display max-id) (newline)))))

(main)
