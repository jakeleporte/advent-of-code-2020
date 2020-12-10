#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim))

(define window-length 25)

(define (valid? window target)
  "Determine if a given TARGET is the sum of any two
numbers in the input list WINDOW"
  (define diffs (make-hash-table))
  (call/cc (lambda (return)
     (for-each (lambda (e) (if (hash-ref diffs e)
			       (return #t)
			       (hash-set! diffs (- target e) #t)))
	       window) #f)))

(define (main)
  (let loop ((window '()))
    (if (< (length window) window-length)
	(loop (append window (list (string->number (read-line)))))
	(let ((num (string->number (read-line))))
	  (if (not (valid? window num))
	      (begin (display num) (newline))
	      (loop (append (cdr window) (list num))))))))

(main)
