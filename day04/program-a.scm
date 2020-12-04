#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (srfi srfi-1))

;; Required passport fields
(define required-fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

(define (field-name str)
  (car (string-tokenize str
			(char-set-complement (char-set #\:)))))

(define (field-value str)
  (cadr (string-tokenize str
			 (char-set-complement (char-set #\:)))))

(define (filled-field? str)
  (not (string-null? (field-value str))))

(define (read-passport-fields)
  "For a passport on stdin, determine which filled fields
it contains and return the names in a list"
  (let read-loop ((line (read-line)) (fields '()))
    (if (or (eof-object? line) (string-null? line))
	fields
	;; Read each new field into the passport
	;; Filter out those that are not filled
	(let ((filled-fields (filter filled-field? (string-tokenize line))))
	  (read-loop (read-line)
		     (append fields (map field-name filled-fields)))))))

  (define (main)
    (let loop ((passport-fields (read-passport-fields))
	       (count 0))
      (if (not (null? passport-fields))
	  ;; Check if required-fields is a subset of passport-fields
	  (if (lset<= string=? required-fields passport-fields)
	      (loop (read-passport-fields) (1+ count))
	      (loop (read-passport-fields) count))
	  (begin (display count) (newline)))))

(main)
