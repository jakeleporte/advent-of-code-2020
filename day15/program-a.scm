#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 receive))

(define (read-starting-numbers)
  (define last-spoken (make-hash-table))
  (define count (make-hash-table))
  (let* ((numbers (map string->number
		      (string-tokenize (read-line) char-set:digit)))
	 (index (length numbers)))
    (for-each (lambda (number i)
		(hash-set! last-spoken number i)
		(hash-set! count number (1+ (hash-ref count number 0))))
	      numbers (iota index 1 1))
    (values (1+ index)
	    (if (> (hash-ref count (car numbers)) 1)
		(- index (hash-ref last-spoken (car numbers)))
		0)
	    last-spoken)))

(define (recitation target)
  (receive (i number last-spoken) (read-starting-numbers)
    (let loop ((index i) (curr number))
      (let* ((last-ref (hash-ref last-spoken curr))
	     (next (if last-ref (- index last-ref) 0)))
	(hash-set! last-spoken curr index)
	(if (= index target)
	    curr
	    (loop (1+ index) next))))))

(define (main)
  (display (recitation 2020)) (newline))

(main)
