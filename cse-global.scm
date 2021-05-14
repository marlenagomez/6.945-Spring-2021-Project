#|
Alternate CSE Playground
("Global" CSE)

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 10 May 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

(load "library-matcher")

;;; returns a symbol made from concatenating str and num
(define (make-symbol str num)
  (string->symbol (string-append str (number->string num))))

;;; returns a (very dumb) matcher version of a chunk of
;;; scheme code
;;; (TODO: use infer-program-types to make this smarter)
(define (matchify chunk)
  (cond ((integer? chunk) `(? ,(make-symbol "x" chunk) ,number?))
	((symbol? chunk) `(? ,chunk ,symbol?))
	((and (list? chunk) (not (null? chunk)))
	 (append `(,(car chunk)) (map matchify (cdr chunk))))
	(else chunk)))

;;; iterates over some scheme code and finds matching
;;; sequences. returns a useful library of sequences that
;;; repeated more than once.
(define (iterate-over code skip-over)
  
  ;; helpful procedures
  (define local-lib (setup-library 'trie))
  (define useful-lib (setup-library 'trie))
  (define incr 0)
  (define (generate-expr-name)
    (set! incr (+ incr 1))
    (make-symbol "expr-" incr))
  (define (depth list) 
    (if (not (list? list))
	0
	(+ 1 (fold + 0 (map depth list)))))
  
  ;; check-then-add each chunk to the local library
  (for-each (lambda (chunk)
	      (let ((check (find-in local-lib chunk)))
		(if (null? check)
		    (add-local-to local-lib 
				  (generate-expr-name) 
				  (matchify chunk))      ; chunk doesn't match anything, add to local lib
		    (add-local-to useful-lib 
				  (caar check) 
				  (matchify chunk)))))   ; found a match: add this to the useful library
	    ;; sort relevant chunks in order or increasing size
	    (sort (filter (lambda (c)
			    (not (memv (car c) skip-over)))
			  (chunkify code))
		  (lambda (a b) (< (depth a) (depth b)))))
  useful-lib)

;;; the entry point into the global cse recursively runs the iteration 
;;; (above), compressing the scheme-code using the useful library
;;; returned by the iterator, stopping when no useful compress can occur.
(define (run-cse scheme-code)
  (define findings '())
  (define (recursive-run scheme-code found-key-list)
    (let ((useful-lib (iterate-over scheme-code found-key-list)))
      (let ((key-list (browse useful-lib)))
	(if (null? key-list)
	    (cons findings scheme-code)
	    (begin
	      (set! findings 
		    (append findings
			    (map (lambda (key)
				   (cons key `(,(lookup useful-lib key))))
				 key-list)))
	      (recursive-run (compress useful-lib scheme-code) 
			     (append key-list found-key-list)))))))
  (recursive-run scheme-code '()))


; ---  testing  ---

(run-cse '(+ 1 2 3))
; -> (() (+ 1 2 3))

(run-cse '(+ (* x 3) (- x y) (* x 3) (- x y)))
; w/out matchify -> (+ (expr-2) (expr-3) (expr-2) (expr-3))
; w/matchify -> (+ (expr-1 x 3) (expr-2 x y) (expr-1 x 3) (expr-2 x y))
 
(run-cse '(lambda (x)
	    (/ (+ (* x 3) (- y z) (- x y) (* x 3))
	       (- y z))))
; w/out matchify: -> (lambda (x) (/ (+ (expr-5) (expr-6) (- x y) (expr-5)) (expr-6)))
; with matchify: -> (lambda (x) (/ (+ (expr-2 x 3) (expr-3 y z) (expr-3 x y) (expr-2 x 3)) (expr-3 y z)))

(run-cse '(+ (* (+ a b c) (- a b c))
	     (* (+ a b c) (- a b c))))
; w/out matchify -> (+ (expr-2) (expr-2))
; with matchify -> (+ (expr-3 a b c) (expr-3 a b c))

(run-cse '(+ (* (+ a b c) (- a b c))
	     (* (+ x y z) (- x y z)))) 
; w/out matchify -> (+ (* (+ a b c) (- a b c)) (* (+ x y z) (- x y z))) 
; with matchify -> (+ (expr-3 a b c) (expr-3 x y z))


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







