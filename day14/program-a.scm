#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 match))

(define (apply-mask mask int)
  "Given a MASK as a string, ignore bits in INT corresponding
to 'X' in the MASK, and for each '0' or '1' in the MASK, replace
the corresponding bit in INT"
  (let ((bits (string->list
	       (string-pad (number->string int 2) 36 #\0)))
	(bitmask (string->list mask)))
    (string->number
     (list->string
      (map (lambda (c1 c2) (match c1 (#\X c2) (bit bit)))
	   bitmask bits))
     2)))

(define (run-docking-program)
  (define memory (make-hash-table))
  (let loop ((line (read-line)) (mask ""))
    (if (eof-object? line) memory
	(let ((inst (string-tokenize line char-set:letter+digit)))
	  (match (car inst)
	    ("mask" (loop (read-line) (cadr inst)))
	    (_ (hash-set! memory (cadr inst)
			  (apply-mask mask (string->number (caddr inst))))
	       (loop (read-line) mask)))))))

(define (main)
  (let ((memory (run-docking-program)))
    (display
     (hash-fold (lambda (key value prev) (+ value prev))
		0 memory))
    (newline)))

(main)
