#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 regex) (ice-9 peg))

(define-syntax macro-literal-insert
  (syntax-rules ()
    ((macro-literal-insert macro var)
     (eval `(macro ,var) (interaction-environment)))))

(define (read-rules)
  (let loop ((line (read-line)) (rules ""))
    (if (string-null? line)
	(process-rules rules)
	(loop (read-line) (string-append rules line "\n")))))

(define (process-rules rules)
  (define peg-rules
    (regexp-substitute/global #f " ([0-9 ]+) \\| ([0-9 ]+)" rules
			      'pre " (" 1 ") / (" 2 ")" 'post))
  (set! peg-rules
    (regexp-substitute/global #f "[0-9]+" peg-rules 'pre "r" 0 'post))
  (set! peg-rules
    (regexp-substitute/global #f ":" peg-rules 'pre " <" 'post))
  (set! peg-rules
    (regexp-substitute/global #f "\"" peg-rules 'pre "'" 'post))
  peg-rules)

(define (count-messages)
  (let loop ((line (read-line)) (count 0))
    (if (eof-object? line) count
	(let* ((m (match-pattern r0 line)))
	  (if (and m (string=? line (peg:substring m)))
	      (loop (read-line) (1+ count))
	      (loop (read-line) count))))))

(define (main)
  (let ((rules (read-rules)))
    (macro-literal-insert define-peg-string-patterns rules)
    (display (count-messages)) (newline)))

(main)
