#|
Trie Matching

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 1 May 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

;;;; Expanded from SDF's pattern and graph matchers

(load "sdf/manager/load")
(manage 'new 'term)
(manage 'add 'design)
;(manage 'add 'unification)
;(manage 'add 'pattern-matching-on-graphs)

(define (tmatch:extend-with-path-value value dictionary)
  (match:extend-dict '(? $value) value dictionary)) ; reserving special keyword $value to point to a found path's value

(define (tmatch:edge trie-edge)
  (let ((predicate (car trie-edge)))      ; assuming trie predicates return the edge value, for this to work
    (let ((edge-pattern (predicate 'throw-away)))
      (cond ((match:var? edge-pattern)
	     (case (match:var-type edge-pattern)
	       ((?) (match:element edge-pattern))
	       (else (error "Unknown var type:" edge-pattern))))
	    (else
	     (match:eqv edge-pattern))))))

(define (tmatch:or trie)
  (lambda (object dict succeed)
    (let loop ((edges (trie-edge-alist trie)))
      (if (pair? edges)
          (or ((tmatch:edge (car edges)) 
	       object 
	       dict 
	       (lambda (dict1 n)
		 (let ((next-trie (cdar edges)))
		   (let ((path-value (trie-has-value? next-trie)))
		     (if path-value
			 (succeed 
			  (tmatch:extend-with-path-value path-value dict1)
			  1)
			 ((tmatch:or (cdar edges)) 
			  (list-tail object n) dict1 succeed))))))
              (loop (cdr edges)))
          #f))))

(define (tmatch:compile-trie trie)
  (tmatch:trie trie))

(define (tmatch:trie trie)
  (define (trie-match data dictionary succeed)
    (if (pair? data)
	((tmatch:or trie)
	 (car data)
	 dictionary
	 (lambda (final-dictionary n)
	   (succeed final-dictionary 1)))
	#f))
  trie-match)

;;; Trie Matcher User Interface
(define (t:matcher trie)
  (let ((match-procedure (tmatch:compile-trie trie)))
    (lambda (datum)
      (run-matcher match-procedure
		   datum
		   match:bindings))))


; ---------- TESTING ----------
#|
(define test-trie (make-trie))

(set-path-value! test-trie (list (lambda (args) 'x)
				 (lambda (args) 'y)
				 (lambda (args) 'z))
		 'xyz-path)

(set-path-value! test-trie (list (lambda (args) 'x)
                                 (lambda (args) `(? y ,number?))
                                 (lambda (args) 'z))
                 'xuz-path)

((t:matcher test-trie) '(x y z))
; -> (($value xyz-path ?)) 

((t:matcher test-trie) '(x 9 z))
; -> (($value xuz-path ?) (y 9 ?))

((t:matcher test-trie) '(x 3 c))
; -> Value: #f

(set-path-value! test-trie (list (lambda (args) 'a)
                                 (lambda (args) `(? y ,symbol?))
                                 (lambda (args) 'b)
				 (lambda (args) `(? y ,symbol?)))
                 'multi-y-path)

((t:matcher test-trie) '(a x b x))
; -> (($value multi-y-path ?) (y x ?))

((t:matcher test-trie) '(a x b y))
; -> Value: #f

|#
