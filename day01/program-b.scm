#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (srfi srfi-1))

(define (read-numbers)
  "Read in a list of numbers from stdin"
  (do ((line (read-line) (read-line))
       (numbers '() (append numbers (list (string->number line)))))
      ((eof-object? line) numbers)))

(define (find-pair sum numbers)
  "Solve the two number sum problem in O(n)"
  (define diffs (make-hash-table))
  (do ((number (car numbers) (car rest))
       (rest (cdr numbers) (cdr rest)))
      ((or (hash-ref diffs number) (null? rest))
       (if (hash-ref diffs number) (list number (- sum number)) #f))
    (hash-set! diffs (- sum number) #t)))

(define (find-triplet sum numbers)
  "Solve the three number sum problem in O(n^2)"
  (do ((number (car numbers) (car rest))
       (rest (cdr numbers) (cdr rest))
       (pair #f (find-pair (- sum (car rest)) numbers)))
      ((or pair (null? rest))
       (if pair (append pair (list number)) #f))))

(define (main)
  (let ((numbers (read-numbers)))
    (display (reduce * 1 (find-triplet 2020 numbers))) (newline)))

(main)
