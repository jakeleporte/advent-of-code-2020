#!/usr/bin/env guile
!#

;;; Solution using character sets
;;; Hacky, but also the most convenient sounding idea
;;; I could come up with

(use-modules (ice-9 rdelim) (ice-9 regex))

(define rules-rx
  (make-regexp "(.+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)"))

(define-syntax call/ret
  (syntax-rules ()
    ((call/ret exp)
     (call/cc (lambda (ret) exp)))))

(define (read-rules)
  "Read in a list of rules and return a character set
representing the allowed values for each ticket"
  (let loop ((line (read-line))
	     (allowed char-set:empty))
    (if (string-null? line) allowed
	(let* ((rule-m (regexp-exec rules-rx line))
	       (set-1 (ucs-range->char-set
		       (string->number (match:substring rule-m 2))
		       (1+ (string->number (match:substring rule-m 3)))))
	       (set-2 (ucs-range->char-set
		       (string->number (match:substring rule-m 4))
		       (1+ (string->number (match:substring rule-m 5))))))
	  (loop (read-line)
		(char-set-union allowed set-1 set-2))))))

(define (sum-invalid-nums allowed ticket)
  "Return the sum of the numbers in TICKET corresponding to
characters not in ALLOWED"
  (apply + (filter
	    (lambda (n)
	      (not (char-set-contains? allowed (integer->char n))))
	    ticket)))

(define (main)
  (define allowed (read-rules))
  ;; Skip the next 4 lines
  (for-each (lambda (_) (read-line)) (iota 4))
  (let loop ((line (read-line))
	     (sum 0))
    (if (eof-object? line)
	(begin (display sum) (newline))
        (loop (read-line)
	      (+ sum
		 (sum-invalid-nums
		  allowed
		  (map string->number
		       (string-tokenize line char-set:digit))))))))

(main)
