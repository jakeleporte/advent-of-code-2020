#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 regex) (srfi srfi-1))

(define outer-regexp
  (make-regexp "([a-z]+ [a-z]+) bags"))

(define rule-regexp
  (make-regexp "([0-9]+) ([a-z]+ [a-z]+) bags?"))

(define (read-rules)
  "Read in rules and make a hash table of 
color: list of (count . contained-color)"
  (define rules (make-hash-table))
  (let loop ((line (read-line)))
    (if (eof-object? line)
	rules
	(begin
	  (let ((outer-color
		 (match:substring (regexp-exec outer-regexp line) 1))
		(number-list (map (lambda (m)
				    (string->number (match:substring m 1)))
				 (list-matches rule-regexp line)))
		(color-list (map (lambda (m) (match:substring m 2))
				 (list-matches rule-regexp line))))
		(hash-set! rules outer-color (map cons number-list color-list)))
	  (loop (read-line))))))

(define (count-bags rules root)
  "Recursively count bags contained in a given color bag;
Assumes no cycles in rules"
  (let* ((bags (hash-ref rules root '()))
	 (bag-count (reduce + 0 (map car bags))))
    (if (null? bags)
	0
	(fold + bag-count
	      (map (lambda (b)
		     (* (car b) (count-bags rules (cdr b))))
		   bags)))))

(define (main)
  (let ((rules (read-rules)))
    (display (count-bags rules "shiny gold"))
    (newline)))

(main)
