#|
Function Library

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 24 April 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

;;; Dependencies
(load "library-utils")
(load "trie-matcher")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fnc-library (make-trie))

(define (add-to-library name proc)
  (set-path-value! fnc-library (tmatcher:proc->path proc) name)
  (add-parameter-list! name (gather-parameters proc))
  (bind-proc name proc))

(define (find-in-library proc)
  (let ((match-dict ((t:matcher fnc-library) proc)))
    (if (not (cdr match-dict))
	'()
	(let ((value (safe-match-value-lookup tmatch:path-value-keyword 
					      match-dict)))
	  (if (not value) 
	      '()
	      (let ((matched-params (map-parameters (get-parameters value) match-dict)))
		`((,value ,@matched-params))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal (helper) procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define parameter-table (make-alist-store eq?))
(define (add-parameter-list! name params) ((parameter-table 'put!) name params))
(define (get-parameters name) ((parameter-table 'get) name))
(define (has-parameters? name)((parameter-table 'has?) name))

;;; Converts a proc to a path of predicates suitable for the library trie
(define (tmatcher:proc->path proc)
  (map (lambda (elem)
	 (if (and (list? elem) (not (is-parameter? elem)))
	     (let ((inner-trie (make-trie)))
	       (intern-path-trie inner-trie (tmatcher:proc->path elem))
	       (lambda (args) inner-trie))
	     (lambda (args) elem)))
       proc))

#|
;;; TODO extension: infer the parameters?
(infer-program-types '(+ 1 2 3))
; ***type-error***
(infer-program-types '(lambda (x) (modulo x 3)))
#|
(t (type:procedure ((? x:4)) (? type:7))
   (infer-program-types (modulo 'x 3))
   (lambda (x) (t (? type:7) 
		  ((t (type:procedure ((? x:4) (numeric-type)) 
				      (? type:7)) modulo) 
		   (t (? x:4) x) (t (numeric-type) 3)))))
|#
(infer-program-types '(modulo x 3))
#|
(t (? type:10) 
   ((t (type:procedure ((? x:8) (numeric-type)) (? type:10)) modulo) 
    (t (? x:8) x) 
    (t (numeric-type) 3)))
|#
|#

; --------------------------------

#|
; ... testing library ...

(add-to-library '123-test '(+ 1 2 3))
(find-in-library '(+ 1 2 3))          ; -> (|123-test|)
(find-in-library '(+ 1 2 4))          ; -> ()

(add-to-library 'mod10 `(modulo (? x ,symbol?) 10))
(find-in-library '(modulo y 10))      ; -> (mod10)

(trie-edge-alist fnc-library)
; ... testing bing-proc ...

(bind-proc 'test-name '(+ 1 2 3))
(test-name) ; -> 6

(bind-proc 'test-lambda '(lambda (x) (+ x 2)))
(test-lambda)     ; -> #[compound-procedure 19]
((test-lambda) 1) ; -> 3

;(bind-proc 'test-name2 '(define (x y) (pp y)))
;(test-name2)          ; -> x
;((test-name2) 'hello) ; -> The object x is not applicable. *TODO*

; ... testing proc->path ...

(define test-proc '(+ 1 2 3))

(define proc->path:test (proc->path test-proc))

(define trie-path (intern-path-trie fnc-library proc->path:test))

(set-path-value! fnc-library proc->path:test 'trie-path)

;;; demonstration: get-a-value given a (list) proc should return the (procedure) name
(get-a-value fnc-library '(+ 1 2 3)) 
; -> trie-path

; --------------------------------
|#



















