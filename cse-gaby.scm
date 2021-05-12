#|
Alternate CSE Playground

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 10 May 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

(load "library-matcher")

(define (iterate-over code skip-over)
  ;; helpful procedures
  (define local-lib (setup-library 'trie))
  (define useful-lib (setup-library 'trie))
  (define incr 0)
  (define (generate-expr-name)
    (set! incr (+ incr 1))
    (string->symbol (string-append "expr-" (number->string incr))))
  (define (depth list) 
    (if (not (list? list))
	0
	(+ 1 (fold + 0 (map depth list)))))
  
  ;; check-then-add each chunk to the local library
  (for-each (lambda (chunk)
	      (let ((check (find-in local-lib chunk)))
		(pp (list "checking" chunk))
		(if (null? check)
		    (add-local-to local-lib (generate-expr-name) chunk)
		    (add-local-to useful-lib (caar check) chunk))))
	    ;; sort relevant chunks in order or increasing size
	    (sort (filter (lambda (c)
			    (not (memv (car c) skip-over)))
			  (chunkify code))
		  (lambda (a b) (< (depth a) (depth b)))))
  useful-lib)

(define (run-depth-rounds scheme-code found-key-list)
  ) ;; TODO

(define (run-cse scheme-code)
  (define (recursive-run scheme-code found-key-list)
    (let ((useful-lib (iterate-over scheme-code found-key-list)))
      (let ((key-list (browse useful-lib)))
	(if (null? key-list)
	    scheme-code
	    (begin 
	      (pp (compress useful-lib scheme-code))
	      (recursive-run (compress useful-lib scheme-code) 
			     (append key-list found-key-list)))))))
  (recursive-run scheme-code '()))



;;; TODO extension: infer the parameters?
(load "sdf/unification/type-resolver")
(load "sdf/unification/unify")

(infer-program-types '(+ 1 2 3))
; ***type-error***
(infer-program-types '(lambda (x) (modulo x 3)))
#|
(t (type:procedure ((? x:4)) (? type:7))
   (infer-program-types (modulo 'x 3))
   (lambda (x) (t (? type:7)
                  ((t (type:procedure ((? x:4) (numeric-type))
                                      (? type:7)) modulo)
                   (t (? x:4) x) 
		   (t (numeric-type) 3)))))
|#
(infer-program-types '(modulo x 3))
#|
(t (? type:10)
   ((t (type:procedure ((? x:8) (numeric-type)) (? type:10)) modulo)
    (t (? x:8) x)
    (t (numeric-type) 3)))
|#






(run-cse '(+ 1 2 3))
; -> (+ 1 2 3)

(run-cse '(+ (* x 3) (- x y) (* x 3) (- x y)))
; -> (+ (expr-2) (expr-3) (expr-2) (expr-3))

(run-cse '(lambda (x)
	    (/ (+ (* x 3) (- y z) (- x y) (* x 3))
	       (- y z))))
; -> (lambda (x) (/ (+ (expr-5) (expr-6) (- x y) (expr-5)) (expr-6)))

(run-cse '(+ (* (+ a b c) (- a b c))
	     (* (+ a b c) (- a b c))))
; -> (+ (expr-2) (expr-2))
