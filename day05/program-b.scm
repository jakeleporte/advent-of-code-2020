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
  (define seat-bits (make-bitvector 1024 #f))
  (let loop ((seat-id (read-seat-id)))
    (if seat-id
	(begin (bitvector-set-bit! seat-bits seat-id)
	       (loop (read-seat-id)))))
  ;; Find the first empty seat after the first filled seat
  (display (bitvector-position seat-bits #f (bitvector-position seat-bits #t 0)))
  (newline))

(main)
