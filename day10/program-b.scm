#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim))

(define (read-input)
  "Read in a list of joltages from stdin and
return the sorted list"
  (let loop ((line (read-line)) (joltages '(0)))
    (if (eof-object? line) joltages
	(loop (read-line) (cons (string->number line) joltages)))))

(define (count-ways joltages start cache)
  "Compute the number of arrangements recursively, and use
a hash table for memoization"
  (cond
   ((hash-ref cache start) (hash-ref cache start))
   ((= start 0) 1)
   (else
    (let ((next (filter (lambda (e)
			  (and (< (- start e) 4) (> (- start e) 0)))
			joltages)))
      (hash-set!
       cache start (apply
		    +
		    (map (lambda (e)
			   (count-ways (delete e joltages) e cache)) next)))
      (hash-ref cache start)))))

(define (main)
  (let ((joltages (read-input)))
    (display
     (count-ways joltages (+ (apply max joltages) 3) (make-hash-table)))
    (newline)))

(main)
