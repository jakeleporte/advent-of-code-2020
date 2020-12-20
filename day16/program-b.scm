#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 regex) (srfi srfi-1) (srfi srfi-43))

(define rules-rx
  (make-regexp "(.+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)"))

(define (make-num-set intervals)
  "Return an interval object with the list of pairs ALLOWED
determining membership in the interval"
  (let ((ranges intervals))
    (define (valid? num)
      (fold (lambda (range prev)
	      (or (and (>= num (car range))
		       (<= num (cdr range))) prev))
	    #f ranges))
    (define (get-ranges)
      ranges)

    (lambda args
      (apply
       (case (car args)
	 ((valid?) valid?)
	 ((get-ranges) get-ranges)
	 (else (error "Invalid method")))
       (cdr args)))))

(define (read-rules)
  "Read in rules and return a hash table of field->char-set
representing the allowed values for each field on a ticket"
  (define rules (make-hash-table))
  (let loop ((line (read-line)))
    (if (string-null? line) rules
	(let* ((rule-m (regexp-exec rules-rx line))
	       (field (match:substring rule-m 1))
	       (set-1 (cons
			 (string->number (match:substring rule-m 2))
			 (string->number (match:substring rule-m 3))))
	       (set-2 (cons
			 (string->number (match:substring rule-m 4))
			 (string->number (match:substring rule-m 5)))))
	  (hash-set! rules field (make-num-set (list set-1 set-2)))
	  (loop (read-line))))))

(define (rules->allowed rules)
  "Take a hash table of RULES and return a num-set
of allowed values"
  (make-num-set
   (hash-fold (lambda (key value prev)
		(append (value 'get-ranges) prev)) '() rules)))

(define (ticket-valid? num-set ticket)
  "Return #t if every number in TICKETS corresponds to a character
in ALLOWED, else #f"
  (if (null? ticket) #t
      (and (num-set 'valid? (car ticket))
	   (ticket-valid? num-set (cdr ticket)))))

(define (read-tickets num-set)
  "Read an array of valid tickets from stdin"
  (let loop ((line (read-line))
	     (tickets '()))
    (if (eof-object? line) tickets
	(let ((ticket (map string->number
			   (string-tokenize line char-set:digit))))
	  (if (ticket-valid? num-set ticket)
	      (loop (read-line) (cons (list->vector ticket) tickets))
	      (loop (read-line) tickets))))))

(define (possible-fields rules tickets)
  (define fields (hash-map->list (lambda (key value) key) rules))
  (define fields-map (make-vector (vector-length (car tickets)) fields))
  (let loop ((rest tickets))
    (if (null? rest) fields-map
	;; For each ticket, remove possible fields if the
	;; value in that ticket is invalid for that field
	(begin
	  (vector-for-each
	   (lambda (i num)
	     (hash-for-each-handle
	      (lambda (handle)
		(unless ((cdr handle) 'valid? num)
		  (vector-set!
		   fields-map i
		   (delete (car handle) (vector-ref fields-map i)))))
	      rules))
	   (car rest))
	  (loop (cdr rest))))))

(define (map-fields possible-fields-map)
  (define fields-map (vector-copy possible-fields-map))
  (while (vector-any (lambda (fields) (> (length fields) 1)) fields-map)
    (vector-for-each
     (lambda (i e)
       (when (= (length e) 1)
	 (vector-for-each
	  (lambda (j fields)
	    (when (not (= j i))
	      (vector-set!
	       fields-map j
	       (delete (car (vector-ref fields-map i)) fields))))
	  fields-map)))
     fields-map))
  (vector-map
   (lambda (i e) (car e)) fields-map))

(define (starts-with field-map str ticket)
  (map cadr
   (filter
    (lambda (e)
      (= (string-prefix-length
	  str
	  (vector-ref field-map (car e)))
	 (string-length str)))
    (zip (iota (vector-length ticket)) (vector->list ticket)))))

(define (main)
  (define rules (read-rules))
  (define allowed (rules->allowed rules))
  ;; Skip the next line and read your ticket
  (define my-ticket
    (begin (read-line)
	   (list->vector
	    (map
	     string->number
	     (string-tokenize (read-line) char-set:digit)))))
  ;; Skip two lines and read in the nearby tickets
  (define tickets (begin (read-line) (read-line) (read-tickets allowed)))
  (define field-map (map-fields (possible-fields rules tickets)))
  (display (apply * (starts-with field-map "departure" my-ticket))) (newline))

(main)
