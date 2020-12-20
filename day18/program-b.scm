#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 regex))

(define paren-rx (make-regexp "\\(([^\\(\\)]+)\\)"))
(define oper-rx (make-regexp "[0-9]+ \\+ [0-9]+"))

(define (eval-line line)
  "Evaluate a line of math, parentheses first, then addition,
then multiplication"
  (let ((pm (regexp-exec paren-rx line))
	(om (regexp-exec oper-rx line)))
    (cond (om
	   (eval-line
	    (regexp-substitute #f om
			       'pre
			       (number->string (%eval-line (match:substring om 0)))
			       'post)))
	  (pm
	   (eval-line
	    (regexp-substitute #f pm
			       'pre
			       (number->string (%eval-line (match:substring pm 1)))
			       'post)))
	  (else (%eval-line line)))))

(define (%eval-line line)
  (let loop ((val (string->number (car (string-tokenize line))))
	     (prob (cdr (string-tokenize line))))
    (if (null? prob) val
	(loop
	 ((eval (string->symbol (car prob)) (interaction-environment))
	  val (string->number (cadr prob)))
	 (cdr (cdr prob))))))

(define (sum-answers)
  (let loop ((line (read-line))
	     (sum 0))
    (if (eof-object? line) sum
	(loop (read-line) (+ sum (eval-line line))))))

(define (main)
  (display (sum-answers)) (newline))

(main)
