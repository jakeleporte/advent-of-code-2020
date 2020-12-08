#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 regex))

(define rule-regexp
  (make-regexp "([[:lower:]]+ [[:lower:]]+) bags?"))

(define (read-rules)
  "Read in rules and make a hash table of color: list of colors
that CAN CONTAIN that color bag"
  (define rules (make-hash-table))
  (let loop ((line (read-line)))
    (if (eof-object? line)
	rules
	(begin
	  (let ((color-list (map (lambda (m) (match:substring m 1))
				 (list-matches rule-regexp line))))
	    (for-each (lambda (color)
			(hash-set! rules color
				   (if (hash-ref rules color)
				       (cons (car color-list) (hash-ref rules color))
				       (list (car color-list)))))
		      (cdr color-list)))
	  (loop (read-line))))))

(define (count-nodes rules root)
  "Count nodes reachable from a given root node"
  (define visited (make-hash-table))
  (let loop ((node root))
    (unless (hash-ref visited node)
      (hash-set! visited node #t)
      (for-each loop (hash-ref rules node '()))))
  (hash-count (const #t) visited))

(define (main)
  (let ((rules (read-rules)))
    (display (1- (count-nodes rules "shiny gold")))
    (newline)))

(main)
