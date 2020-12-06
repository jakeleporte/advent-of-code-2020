#!/usr/bin/env guile
!#

;;; Using lset

(use-modules (ice-9 rdelim) (srfi srfi-1))

(define (process-customs-form-b)
  (define first-line (read-line))
  (if (eof-object? first-line) #f
      (let loop ((line first-line)
		 (letters (string->list "abcdefghijklmnopqrstuvwxyz")))
	(if (or (eof-object? line) (string-null? line))
	    letters
	    (loop (read-line)
		  (lset-intersection char=? letters (string->list line)))))))

(define (main)
  (let loop ((letters (process-customs-form-b))
	     (count 0))
    (if letters
      (loop (process-customs-form-b) (+ count (length letters)))
      (begin (display count) (newline)))))

(main)
