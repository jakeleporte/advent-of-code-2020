#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 receive) (ice-9 match))

(define (read-code)
  "Read code and return vector of (op . arg) pairs"
  (let loop ((line (read-line)) (code '()))
    (if (eof-object? line)
	(list->vector (append code (list '("hlt" . 0))))
	(let* ((inst (string-tokenize line))
	       (op (car inst))
	       (arg (string->number (cadr inst))))
	  (loop (read-line)
		(append code (list (cons op arg))))))))

(define (run-code code)
  "Executes a series of instructions and returns
the accumulator value; if the program does not halt,
return #f (suck it Turing)"
  (define visited (make-hash-table (vector-length code)))
  (let loop ((pc 0)
	     (acc 0))
    (if (hash-ref visited pc)
	#f
	(begin
	  (hash-set! visited pc #t)
	  (receive (op arg) (values (car (vector-ref code pc))
				    (cdr (vector-ref code pc)))
	    (match op
	      ("nop" (loop (1+ pc) acc))
	      ("jmp" (loop (+ pc arg) acc))
	      ("acc" (loop (1+ pc) (+ acc arg)))
	      ("hlt" acc)))))))

(define (fix-code code)
  "Run code, changing successive nop and jmp instructions
until it halts and returns a value."
  (let loop ((pc 0))
    ;; Try to modify and run for each value of pc (instruction index)
    (let ((op (car (vector-ref code pc)))
	  (arg (cdr (vector-ref code pc)))
	  (mod-code (vector-copy code)))
      (match op
	;; Modify the code and run
	("nop" (vector-set! mod-code pc (cons "jmp" arg))
	 (let ((ret (run-code mod-code)))
	   (if ret ret (loop (1+ pc)))))
	("jmp" (vector-set! mod-code pc (cons "nop" arg))
	 (let ((ret (run-code mod-code)))
	   (if ret ret (loop (1+ pc)))))
	;; Don't bother running the unmodified code
	(_ (loop (1+ pc)))))))

(define (main)
  (display (fix-code (read-code))) (newline))

(main)
