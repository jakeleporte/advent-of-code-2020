#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (srfi srfi-1) (srfi srfi-9) (ice-9 regex))

(define hgt-regexp (make-regexp "^([[:digit:]]+)(cm|in)$"))
(define hcl-regexp (make-regexp "^(#[[:xdigit:]]{6})$"))
(define ecl-regexp (make-regexp "^amb$|^blu$|^brn$|^gry$|^grn$|^hzl$|^oth$"))
(define pid-regexp (make-regexp "^([[:digit:]]{9})$"))

(define (field-name str)
  (car (string-tokenize str (char-set-complement (char-set #\:)))))

(define (field-value str)
  (cadr (string-tokenize str (char-set-complement (char-set #\:)))))

(define (read-passport)
  "For a passport on stdin, determine which filled fields
it contains and return the names in a list"
  (define passport (make-hash-table 8))
  (let ((first-line (read-line)))
    (if (eof-object? first-line)
	#f
	(let read-loop ((line first-line))
	  (if (or (eof-object? line) (string-null? line))
	      passport
	      ;; Read each new field into the passport
	      (begin
		(for-each (lambda (str) (hash-set! passport
						    (field-name str)
						    (field-value str)))
			  (string-tokenize line))
		(read-loop (read-line))))))))

(define (check-number lower upper input)
  (and (string? input)
       (string->number input)
       (>= (string->number input) lower)
       (<= (string->number input) upper)))

(define (check-regexp regexp input)
  (and (string? input)
       (regexp-exec regexp input)))

(define (check-height cm-lower cm-upper in-lower in-upper input)
  (if (string? input)
      (let ((hgt-match (regexp-exec hgt-regexp input)))
	(if hgt-match
	    (let ((hgt-num (string->number (match:substring hgt-match 1)))
		  (hgt-units (match:substring hgt-match 2)))
	      (or (and (string=? hgt-units "cm")
		       (>= hgt-num cm-lower) (<= hgt-num cm-upper))
		  (and (string=? hgt-units "in")
		       (>= hgt-num in-lower) (<= hgt-num in-upper))))
	    #f))
      #f))

(define (valid-passport? passport)
  (and
   ;; birth year
   (let ((byr (hash-ref passport "byr")))
     (check-number 1920 2002 byr))
   (let ((iyr (hash-ref passport "iyr")))
     (check-number 2010 2020 iyr))
   (let ((eyr (hash-ref passport "eyr")))
     (check-number 2020 2030 eyr))
   (let ((hgt (hash-ref passport "hgt")))
     (check-height 150 193 59 76 hgt))
   (let ((hcl (hash-ref passport "hcl")))
     (check-regexp hcl-regexp hcl))
   (let ((ecl (hash-ref passport "ecl")))
     (check-regexp ecl-regexp ecl))
   (let ((pid (hash-ref passport "pid")))
     (check-regexp pid-regexp pid))))
     

  (define (main)
    (let loop ((passport (read-passport)) (count 0))
      ;; Check if hash table has any fields filled
      (if passport
	  (if (valid-passport? passport)
	      (loop (read-passport) (1+ count))
	      (loop (read-passport) count))
	  (begin
	    (display count) (newline)))))

(main)
