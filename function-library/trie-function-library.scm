#|
Trie Function Library

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 24 April 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

;;; Dependencies
(load "function-library/library-utils")
(load "function-library/trie-matcher")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-trie-library) 
  (cons (make-trie) (make-tagged-alist-store eq?)))

(define (trie-of library) (car library))
(define (lookup-table-of library) (cdr library))

(define (trie-library? library)
  (and (pair? library)
       (trie? (trie-of library))
       (alist-store? (lookup-table-of library))))
(register-predicate! trie-library? 'trie-library?)

(define (add-to-trie library name proc)
  (set-path-value! (trie-of library) (tmatcher:proc->path proc) name)
  (add-to! (lookup-table-of library) name proc)
  (bind-proc name proc)
  library)

(define (add-local-to-trie library name proc)
  (set-path-value! (trie-of library) (tmatcher:proc->path proc) name)
  (add-to! (lookup-table-of library) name proc)
  library)

(define (find-in-trie library proc)
  (let ((match-dict ((t:matcher (trie-of library)) proc)))
    (if (not (cdr match-dict))
	'()
	(let ((value (safe-match-value-lookup tmatch:path-value-keyword 
					      match-dict)))
	  (if (not value) 
	      '()
	      (let ((proc (get value (lookup-table-of library))))
		(let ((matched-params (map-parameters 
				       (gather-parameters proc) 
				       match-dict)))
		  `((,value ,@matched-params)))))))))

(define (lookup-in-trie library name)
  (get name (lookup-table-of library)))

(define (remove-from-trie library name)
  (let ((proc ((get-executable name))))
    (pp (list "removing" proc))
    (pp (list? proc))
    (set-path-value! (trie-of library) (tmatcher:proc->path proc) 'the-nothing-value)
    (remove-from! (lookup-table-of library) name)
    (unbind-proc name)
    library))

(define (get-all-entries-in-trie library)
  ((get-keys-from (lookup-table-of library))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal (helper) procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Converts a proc to a path of predicates suitable for the library trie
(define (tmatcher:proc->path proc)
  (map (lambda (elem)
	 (if (and (list? elem) (not (is-parameter? elem)))
	     (let ((inner-trie (make-trie)))
	       (intern-path-trie inner-trie (tmatcher:proc->path elem))
	       (lambda (args) inner-trie))
	     (lambda (args) elem)))
       proc))

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



















