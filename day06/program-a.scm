#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (srfi srfi-1))

(define (process-customs-form-a)
  (define first-line (read-line))
  (if (eof-object? first-line) #f
      (let loop ((line first-line)
		 (letters char-set:empty))
	(if (or (eof-object? line) (string-null? line))
	    letters
	    (loop (read-line)
		  (char-set-union letters (string->char-set line)))))))

(define (main)
  (let loop ((letters (process-customs-form-a))
	     (count 0))
    (if letters
      (loop (process-customs-form-a) (+ count (char-set-size letters)))
      (begin (display count) (newline)))))

(main)
