#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 receive) (ice-9 match))

(define (read-code)
  "Read code and return vector of (op . arg) pairs"
  (let loop ((line (read-line)) (code '()))
    (if (eof-object? line)
	(list->vector code)
	(let* ((inst (string-tokenize line))
	       (op (car inst))
	       (arg (string->number (cadr inst))))
	  (loop (read-line)
		(append code (list (cons op arg))))))))

(define (find-loop code)
  "Runs the input code until a loop is found
and returns the value of the accumulator"
  (define visited (make-hash-table (vector-length code)))
  (let loop ((pc 0) (acc 0))
    ;; If this instruction has already been visited,
    ;; return the accumulator value
    (if (hash-ref visited pc) acc
	(begin (hash-set! visited pc #t)
	       (receive (op arg) (values (car (vector-ref code pc))
					 (cdr (vector-ref code pc)))
		 (match op
		   ("nop" (loop (1+ pc) acc))
		   ("acc" (loop (1+ pc) (+ acc arg)))
		   ("jmp" (loop (+ pc arg) acc))))))))

(define (main)
  (display (find-loop (read-code))) (newline))

(main)
