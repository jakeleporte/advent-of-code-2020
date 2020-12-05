#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 regex))

(define password-regex
  (make-regexp
   "([[:digit:]]+)-([[:digit:]]+) ([[:alpha:]]): ([[:alpha:]]+)"))

(define (verify-password-b char pos-1 pos-2 pass)
  "Checks that a password contains char in POS1 or POS2"
  ;; Given indices are 1-referenced; change them to 0-referenced
  (let ((in-pos-1 (char=? (string-ref pass (1- pos-1)) char))
	(in-pos-2 (char=? (string-ref pass (1- pos-2)) char)))
    ;; Check for xor- not the same boolean value
    (not (eq? in-pos-1 in-pos-2))))

(define (main)
  (let loop ((line (read-line))
	     (valid 0))
    ;; Loop until EOF
    (if (not (eof-object? line))
	(let* ((pass-match (regexp-exec password-regex line))
	       (min (string->number (match:substring pass-match 1)))
	       (max (string->number (match:substring pass-match 2)))
	       (char (string-ref (match:substring pass-match 3) 0))
	       (pass (match:substring pass-match 4))
	       (count (string-count pass char)))
	  (if (verify-password-b char min max pass)
	      (loop (read-line) (1+ valid))
	      (loop (read-line) valid)))
	(begin (display valid) (newline)))))

(main)
