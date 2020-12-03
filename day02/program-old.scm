#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim))

(define (read-password-line)
  "Read in a line from the password file and return a list of
the required char, the min and max values for it, and the password
itself."
  (define line (read-line))
  (if (not (eof-object? line))
      (let* ((pass-list (string-tokenize line))
	     (min-max (string-tokenize (car pass-list) char-set:digit))
	     (min (string->number (car min-max)))
	     (max (string->number (cadr min-max)))
	     (char (string-ref (cadr pass-list) 0))
	     (pass (caddr pass-list)))
	(list char min max pass))
      #f))

(define (verify-password-a char min max pass)
  "Check that a password contains between MIN and MAX
numbers of CHAR, inclusive"
  (let ((count (string-count pass char)))
    (if (and (>= count min) (<= count max))
	#t
	#f)))

(define (verify-password-b char pos1 pos2 pass)
  "Checks that a password contains char in POS1 or POS2"
  ;; Given indices are 1-referenced; change them to 0-referenced
  (define pos-a (1- pos1))
  (define pos-b (1- pos2))
  (let ((in-pos-a (char=? (string-ref pass pos-a) char))
	(in-pos-b (char=? (string-ref pass pos-b) char)))
    ;; Check for xor- not the same boolean value
    (not (eq? in-pos-a in-pos-b))))

(define (main)
  (let loop ((pass-list (read-password-line))
	     (valid-pass-count 0))
    ;; Loop until pass-list is false
    (if pass-list
	(let ((char (car pass-list))
	      (min (cadr pass-list))
	      (max (caddr pass-list))
	      (pass (cadddr pass-list)))
	  ;; Verify the password and loop
	  (if (verify-password-b char min max pass)
	      (loop (read-password-line) (1+ valid-pass-count))
	      (loop (read-password-line) valid-pass-count)))
	(display valid-pass-count))))

(main)
