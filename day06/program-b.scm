#!/usr/bin/env guile
!#

;;; Using lset

(use-modules (ice-9 rdelim) (srfi srfi-1))

(define (process-customs-form-b)
  (define first-line (read-line))
  (if (eof-object? first-line) #f
      (let loop ((line first-line)
		 (letters char-set:lower-case))
	(if (or (eof-object? line) (string-null? line))
	    letters
	    (loop (read-line)
		  (char-set-intersection letters (string->char-set line)))))))

(define (main)
  (let loop ((letters (process-customs-form-b))
	     (count 0))
    (if letters
      (loop (process-customs-form-b) (+ count (char-set-size letters)))
      (begin (display count) (newline)))))

(main)
