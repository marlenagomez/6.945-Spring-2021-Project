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
(manage 'add 'unification)
(manage 'add 'pattern-matching-on-graphs)

(define (tmatch:extend-with-path-value value dictionary)
  (match:extend-dict `,value '$value dictionary))

(define (tmatch:edge trie-edge)
  (let ((predicate (car trie-edge)))      ; assuming trie predicates return the edge value, for this to work
    (let ((edge-pattern (predicate 'throw-away)))
      (pp (list "edge pattern:" edge-pattern))
      (cond ((match:var? edge-pattern)
	     (case (match:var-type edge-pattern)
	       ((?) (match:element edge-pattern))
	       (else (error "Unknown var type:" edge-pattern))))
	    (else
	     (match:eqv edge-pattern))))))

(define (tmatch:or trie)
  (lambda (object dict succeed)
    (pp (list "trie" trie))
    (let loop ((edges (trie-edge-alist trie)))
      (if (pair? edges)
          (or ((tmatch:edge (car edges)) 
	       object 
	       dict 
	       (lambda (dict1 n)
		 (let ((next-trie (cdar edges)))
		   (let ((path-value (trie-has-value? next-trie)))
		     (pp (list "next-trie" next-trie))
		     (pp (list "path value" path-value))
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
    (and (pair? data)
	 (let lp ((data-list (car data))
		  (curr-trie trie)
		  (dictionary dictionary))
	   (pp (list "tmatch:trie curr-trie" curr-trie))
	   (if ((tmatch:or curr-trie)
		data-list
		dictionary
		succeed)
	       (succeed dictionary 1)
	       #f))))
  trie-match)

#|
	   (cond ((list? (trie-edge-alist trie))
		  ((tmatch:or curr-trie)
		   data-list
		   dictionary
		   (lambda (new-dictionary n)
		     (if (> n (length data-list))
			 (error "Matcher ate too much." n))
		     (lp (list-tail data-list n)
			 ;;; NEED METHOD OF GRABBING THE MATCHED NEXT TRIE
			 new-dictionary))))
		  ((pair? data-list) #f) ;unmatched data
		  ((null? data-list)
		   (succeed dictionary 1))
		  (else #f)))))
  trie-match)
|#

(define (t:matcher trie)
  (let ((match-procedure (tmatch:compile-trie trie)))
    (lambda (datum)
      (run-t:matcher match-procedure
		     datum
		     match:bindings))))

(define (run-t:matcher match-procedure datum succeed)
  (match-procedure (list datum)
                   (match:new-dict)
                   (lambda (dict n)
                     (and (= n 1)
                          (succeed dict)))))



; ---------- TESTING ----------

(define test-trie (make-trie))

(set-path-value! test-trie (list (lambda (args) 'x)
				 (lambda (args) 'y)
				 (lambda (args) 'z))
		 'xyz-path)

(set-path-value! test-trie (list (lambda (args) 'x)
                                 (lambda (args) `(? y ,number?))
                                 (lambda (args) 'z))
                 'xuz-path)

(run-t:matcher
 (tmatch:compile-trie test-trie)
 '(x y z)
 match:bindings)
 


