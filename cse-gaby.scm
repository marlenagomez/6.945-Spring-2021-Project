#|
Fun CSE Playground

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 10 May 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

(load "library-matcher")

(setup-library 'trie)

(define (iterate-over code skip-over)
  ;; helpful procedures
  (define local-lib (setup-library 'trie))
  (define useful-lib (setup-library 'trie))
  (define incr 0)
  (define (generate-expr-name)
    (set! incr (+ incr 1))
    (string->symbol (string-append "expr-" (number->string incr))))
  
  ;; check-then-add each chunk to the local library
  (for-each (lambda (chunk)
	      (let ((check (find-in local-lib chunk)))
		(if (null? check)
		    (add-local-to local-lib (generate-expr-name) chunk)
		    (add-local-to useful-lib (caar check) chunk))))
	    (filter (lambda (c)
		      (not (memv (car c) skip-over)))
		    (chunkify code)))
  useful-lib)

(define (run-cse scheme-code)
  (define (recursive-run scheme-code found-key-list)
    (let ((useful-lib (iterate-over scheme-code found-key-list)))
      (let ((key-list ((get-keys-from (get-all-entries useful-lib)))))
	(if (null? key-list)
	    scheme-code
	    (begin 
	      (pp (compress useful-lib scheme-code))
	      (recursive-run (compress useful-lib scheme-code) 
			     (append key-list found-key-list)))))))
  (recursive-run scheme-code '()))

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
