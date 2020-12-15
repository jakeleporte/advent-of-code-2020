#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 match))

(define (apply-mask mask int)
  "Given a MASK as a string, return INT as a string of bits, with
'X's where there are 'X's in the MASK"
  (let ((bits (string->list
	       (string-pad (number->string int 2) 36 #\0)))
	(bitmask (string->list mask)))
    (list->string
     (map (lambda (c1 c2) (match c1 (#\0 c2) (#\1 #\1) (#\X #\X)))
	  bitmask bits))))

(define (memory-set! memory addr data)
  (let ((index (string-index addr #\X))
	(addr-0 (string-copy addr))
	(addr-1 (string-copy addr)))
    (if index (begin
		(string-set! addr-0 index #\0)
		(memory-set! memory addr-0 data)
		(string-set! addr-1 index #\1)
		(memory-set! memory addr-1 data))
	(begin
	  (hash-set! memory (string->number addr) data)))))

(define (run-docking-program)
  (define memory (make-hash-table))
  (let loop ((line (read-line)) (mask ""))
    (if (eof-object? line) memory
	(let ((inst (string-tokenize line char-set:letter+digit)))
	  (match (car inst)
	    ("mask" (loop (read-line) (cadr inst)))
	    (_ (memory-set! memory (apply-mask mask (string->number (cadr inst)))
			    (string->number (caddr inst)))
	       (loop (read-line) mask)))))))

(define (main)
  (let ((memory (run-docking-program)))
    (display
     (hash-fold (lambda (key value prev) (+ value prev))
		0 memory))
    (newline)))

(main)
