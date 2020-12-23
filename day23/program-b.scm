#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (srfi srfi-1))

(define (remove-sublist! cyc curr len)
  "Remove a list of length LEN from the circular list
CYC starting after the element eqv? to curr and return 
the removed sublist."
  (let* ((cl (memv curr cyc))
	 (sl (cdr cl)))
    (set-cdr! cl (list-tail cl (1+ len)))
    (set-cdr! (list-tail sl (1- len)) '())
    sl))

(define (insert-sublist! cyc dst sl)
  "Insert the list SL in the circular list CYC after
the element eqv? to DST"
  (let* ((bl (memv dst cyc))
	 (al (cdr bl)))
    (set-cdr! bl sl)
    (set-cdr! (last-pair sl) al)))

(define (find-dst curr len sl)
  (let loop ((dst (1- curr)))
    (cond
     ((= dst 0)
      (loop len))
     ((not (find (lambda (e) (= e dst)) sl))
      dst)
     (else
      (loop (1- dst))))))

(define (crab-cups lst n)
  (define len (length lst))
  (define cyc (apply circular-list lst))
  (let loop ((cl cyc) (moves n))
    (if (= moves 0) cl
	(let* ((curr (car cl))
	       (sl (remove-sublist! cl curr 3))
	       (dst (find-dst curr len sl)))
	  (when (= 0 (modulo moves 1000000))
	    (display moves) (newline))
	  (insert-sublist! cl dst sl)
	  (loop (cdr cl) (1- moves))))))    

(define (main)
  (let* ((il (map
	      (lambda (e) (string->number (string e)))
	      (string->list (read-line))))
	 (rest (iota (- 1000000 (length il)) (1+ (apply max il))))
	 (lst (append il rest))
	 (len (length lst))
	 (rc (crab-cups lst 10000000))
	 (res (cdr (memv 1 rc))))
    (set-cdr! (list-tail res (- len 2)) '())
    (display (* (car res) (cadr res))) (newline)))

(main)
