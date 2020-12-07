#!/usr/bin/env guile
!#

;;; Inneficient (O(n^2)) solution

(use-modules (ice-9 rdelim) (srfi srfi-1))

(define (read-numbers)
  "Read in a list of numbers from stdin"
  (do ((line (read-line) (read-line))
       (numbers '() (append numbers (list (string->number line)))))
      ((eof-object? line) numbers)))

(define (makes-sum? sum number numbers)
  "Check a given number against all numbers in an input list
and return #t if it sums to SUM with any of them"
  (do ((term (car numbers) (car rest))
       (rest (cdr numbers) (cdr rest)))
      ((or (= (+ number term) sum) (null? rest))
       (if (null? rest) #f #t))))

(define (find-pair sum numbers)
  "Solve the two number sum problem in O(n^2)"
  (do ((number (car numbers) (car rest))
       (rest (cdr numbers) (cdr rest)))
      ((or (makes-sum? sum number numbers) (null? rest))
       (list number (- sum number)))))

(define (main)
  (let ((numbers (read-numbers)))
    (display (reduce * 1 (find-pair 2020 numbers))) (newline)))

(main)
