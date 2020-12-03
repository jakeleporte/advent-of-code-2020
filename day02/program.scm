#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 regex))

(define %password-regex
  (make-regexp
   "([[:digit:]]+)-([[:digit:]]+) ([[:alpha:]]): ([[:alpha:]]+)"))

(define (main)
  (let loop ((line (read-line))
	     (valid 0))
    ;; Loop until EOF
    (if (not (eof-object? line))
	(let* ((pass-match (regexp-exec %password-regex line))
	       (min (string->number (match:substring pass-match 1)))
	       (max (string->number (match:substring pass-match 2)))
	       (char (string-ref (match:substring pass-match 3) 0))
	       (pass (match:substring pass-match 4))
	       (count (string-count pass char)))
	  (if (and (>= count min) (<= count max))
	      (loop (read-line) (1+ valid))
	      (loop (read-line) valid)))
	(begin (display valid) (newline)))))

(main)
