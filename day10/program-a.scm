#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 receive))

(define (read-input)
  "Read in a list of joltages from stdin"
  (let loop ((line (read-line)) (joltages '()))
    (if (eof-object? line) joltages
	(loop (read-line) (cons (string->number line) joltages)))))

(define (count-differences joltages)
  "Assume a valid chain can be made, and count the number of 1-jolt
and 3-jolt differences in that chain"
  (let loop ((remaining joltages) (prev 0) (1-diffs 0) (3-diffs 0))
    ;; Your device is always 3 higher than the highes adapter
    (if (null? remaining) (values 1-diffs (1+ 3-diffs))
	(let* ((next (apply min remaining)) (diff (- next prev)))
	  (cond ((= diff 1)
		 (loop (delete next remaining) next (1+ 1-diffs) 3-diffs))
		((= diff 3)
		 (loop (delete next remaining) next 1-diffs (1+ 3-diffs)))
		(else
		 (loop (delete next remaining) next 1-diffs 3-diffs)))))))

(define (main)
  (receive (1-diffs 3-diffs) (count-differences (read-input))
    (display (* 1-diffs 3-diffs)) (newline)))

(main)
