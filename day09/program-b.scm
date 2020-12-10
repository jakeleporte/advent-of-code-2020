#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (srfi srfi-43))

(define window-length 25)

(define (read-vector)
  (let loop ((line (read-line)) (numbers '()))
    (if (eof-object? line)
	(list->vector numbers)
	(loop (read-line)
	      (append numbers (list (string->number line)))))))

(define (valid? window target)
  "Determine if a given TARGET is the sum of any two
numbers in the input vector WINDOW"
  (define diffs (make-hash-table))
  (call/cc
   (lambda (return)
     (vector-for-each
      (lambda (i e) (if (hash-ref diffs e)
		      (return #t)
		      (hash-set! diffs (- target e) #t)))
      window) #f)))

(define (find-invalid-number valid? numbers)
  "Return the first input number in NUMBERS which is not
VALID?"
  (let loop ((start 0) (stop window-length))
    (let ((next (vector-ref numbers stop)))
      (if (valid? (vector-copy numbers start stop) next)
	    (loop (1+ start) (1+ stop))
	    next))))

(define (subvector-sum vec target)
  "Return a subvector that sums to the given TARGET if one exists"
  (define initial-sum (+ (vector-ref vec 0) (vector-ref vec 1)))
  (let loop ((start 0) (end 1) (sum initial-sum))
    (cond ((< sum target) (loop start
				(1+ end)
				(+ sum (vector-ref vec (1+ end)))))
	  ((> sum target) (if (= start end)
			      (loop (1+ start)
				    (1+ end)
				    (vector-ref vec (1+ end)))
			      (loop (1+ start)
				    end
				    (- sum (vector-ref vec start)))))
	  ((= sum target) (vector-copy vec start (1+ end))))))

(define (main)
  (define vec (read-vector))
  (define target (find-invalid-number valid? vec))
  (let ((sublist (vector->list (subvector-sum vec target))))
    (display (+ (apply min sublist) (apply max sublist))) (newline)))

(main)
