#|
Trie Matching

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 1 May 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

;;;; Expanded from SDF's pattern and graph matchers

;;; dependencies
;;; (note: for testing directly, must add ../ before every file

(load "sdf/common/overrides")
(load "sdf/common/collections")
(load "sdf/common/predicates")
(load "sdf/common/generic-procedures")
(load "sdf/common/applicability")
(load "sdf/common/match-utils")
(load "sdf/design-of-the-matcher/matcher")

(load "sdf/common/trie")
(load "sdf/common/predicate-counter")

;;; --- TRIE MATCHER ---

(define tmatch:path-value-keyword '$value)

(define (tmatch:extend-with-path-value value dictionary)
  (match:extend-dict '(? $value) value dictionary)) ; reserving special keyword $value to point to a found path's value

(define (tmatch:trie? pattern) (trie? pattern))

(define (tmatch:edge edge-pattern)
  (cond ((match:var? edge-pattern)
	 (case (match:var-type edge-pattern)
	   ((?) (match:element edge-pattern))
	   (else (error "Unknown var type:" edge-pattern))))
	((tmatch:trie? edge-pattern)
	 (tmatch:trie edge-pattern #f))
	(else
	 (match:eqv edge-pattern))))

(define (tmatch:extract-pattern trie-edge)
  (let ((predicate (car trie-edge)))      
    (predicate 'throw-away)))        ; assuming trie predicates return the edge value, for this to work

(define (children trie) (trie-edge-alist trie))

(define (tmatch:or trie must-end-in-path-value)
  (lambda (object dict succeed)
    (let loop ((edges (children trie)))
      (if (pair? edges)
	  (let ((pattern (tmatch:extract-pattern (car edges))))
	    (or ((tmatch:edge pattern) 
		 object 
		 dict 
		 (lambda (dict1 n)
		   (let ((next-trie (cdar edges)))
		     (let ((path-value (trie-has-value? next-trie)))
		       ;; here, we have matched an edge and must consider
		       ;; 3 options: either we are at a leaf with a path
		       ;; value, a leaf with no path value of an anonymous trie, 
		       ;; or we are not yet at a leaf
		       (cond ((and must-end-in-path-value 
				   path-value
				   (= 1 (length object)))                           ; condition 1: we are done matching
			      (succeed 
			       (tmatch:extend-with-path-value path-value dict1)
			       1))
			     ((and (not must-end-in-path-value) 
				   (not path-value)
				   (= 0 (length (children next-trie))))             ; condition 2: continue matching tries
			      (succeed dict1 1))                                
			     (else ((tmatch:or (cdar edges) must-end-in-path-value) ; condition 3: continue to the next node 
				    (list-tail object n) dict1 succeed)))))))
		(loop (cdr edges))))
	  #f))))

(define (tmatch:compile-trie trie)
  (tmatch:trie trie #t))

(define (tmatch:trie trie anonymous?)
  (define (trie-match data dictionary succeed)
    (if (pair? data)
	((tmatch:or trie anonymous?)
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
      (match:new-bindings 
       (match:new-dict)
       (run-matcher match-procedure
		    datum
		    match:bindings)))))


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

(set-path-value! test-trie (list (lambda (args) 'x)
                                 (lambda (args) `(? y ,number?))
                                 (lambda (args) 'z)
				 (lambda (args) 'z))
                 'xuzz-path)

((t:matcher test-trie) '(x y z))
; -> (dict ($value xyz-path ?)) 

((t:matcher test-trie) '(x 9 z))
; -> (dict ($value xuz-path ?) (y 9 ?))

((t:matcher test-trie) '(x 3 c))
; -> Value: (dict . #f)

((t:matcher test-trie) '(x 9 z z))
; ->  (dict ($value xuzz-path ?) (y 9 ?))

(set-path-value! test-trie (list (lambda (args) 'a)
                                 (lambda (args) `(? y ,symbol?))
                                 (lambda (args) 'b)
				 (lambda (args) `(? y ,symbol?)))
                 'multi-y-path)

((t:matcher test-trie) '(a x b x))
; -> (dict ($value multi-y-path ?) (y x ?))

((t:matcher test-trie) '(a x b y))
; -> Value: (dict . #f)

|#

