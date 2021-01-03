#!/usr/bin/env guile
!#

(use-modules (ice-9 rdelim) (ice-9 receive) (srfi srfi-1))

(define (read-tile)
  "Read an array of characters from stdin and return the
tile id and the tile."
  (define id 0)
  (let ((line (read-line)))
    (if (eof-object? line) (values #f #nil)
	(begin
	  (set! id
	    (string->number (car (string-tokenize line char-set:digit))))
	  (let loop ((tile '()) (line (read-line)))
	    (if (or (eof-object? line) (string-null? line))
		(values id (list->array 2 tile))
		(loop (cons (string->list line) tile) (read-line))))))))

(define (read-tiles)
  "Read tiles from stdin and return a hash table of ID: TILE"
  (define tiles (make-hash-table))
  (let loop ()
    (receive (id tile) (read-tile)
      (if (null? tile) tiles
	  (begin (hash-set! tiles id tile) (loop))))))

(define (edges tile)
  "Build a list of 4 strings representing the four edges 
of the given input TILE character array, in the order top, right,
bottom, left"
  (define dims (array-dimensions tile))
  (define t (transpose-array tile 1 0))
  (define top (array-slice tile 0))
  (define right (array-slice t (1- (cadr dims))))
  (define bottom (array-slice tile (1- (car dims))))
  (define left (array-slice t 0))
  (map (compose list->string array->list) (list top bottom left right)))

(define (tiles->edges tiles)
  "Return a hash table of id: list of tile edges"
  (define tile-edges (make-hash-table))
  (hash-for-each
   (lambda (k v)
     (hash-set! tile-edges k (edges v)))
   tiles)
  tile-edges)

(define (%neighbor tile-edges the-id the-edge)
  "Given THE-EDGE from the tile with id THE-ID, search 
TILE-EDGES for the corresponding edge"
  (call/cc
   (lambda (ret)
     (hash-for-each
      (lambda (id edges)
	(when (not (= id the-id))
	  (for-each
	   (lambda (e)
	     (if (or (string=? the-edge e)
		     (string=? the-edge (string-reverse e)))
		 (ret id)))
	   edges)))
      tile-edges)
     #f)))

(define (edges->neighbors tile-edges)
  "Given a hash table of TILE-EDGES, return a hash table of
id: List of neighbor ids (or #f if there is no neighbor on a side)"
  (define neighbors (make-hash-table))
  (hash-for-each
   (lambda (id edges)
     (hash-set! neighbors
		id
		(map (lambda (e) (%neighbor tile-edges id e)) edges)))
   tile-edges)
  neighbors)

(define (%corner? neighbor-ids)
  "Return #t if the neighbor-id list represents a corner, else #f"
  (= (length (filter identity neighbor-ids)) 2))

(define (corner-ids-product tiles)
  "Build a product of ids representing the tiles that would
go in the corners if the tiles were arranged correctly, with matching
edges lined up."
  (define neighbors (edges->neighbors (tiles->edges tiles)))
  (define product 1)
  (hash-for-each
   (lambda (id neighbor-ids)
     (when (%corner? neighbor-ids)
       (set! product (* product id))))
   neighbors)
  product)

(define (main)
  (let* ((tiles (read-tiles)))
    (display (corner-ids-product tiles)) (newline)
    ))

(main)
