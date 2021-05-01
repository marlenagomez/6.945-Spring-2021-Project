#|
Function Library

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 24 April 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

;;; Dependencies

;;; sdf/efficient-generic/procedures/load-spec-trie
(load "sdf/common/arith")
(load "sdf/common/numeric-arith")
(load "sdf/combining-arithmetics/standard-arith")
(load "sdf/combining-arithmetics/function-variants")
(load "sdf/generic-procedures/generic-arith")
(load "sdf/common/trie")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fnc-library (make-trie))

(define (add-to-library name proc)
  (let ((proc-path (proc->path proc)))
    (set-path-value! fnc-library proc-path name) ; add proc to library
    (bind-proc name proc)))

(define (find-in-library proc)
  (let ((proc-path (proc->path proc)))
    (let ((result (ignore-errors 
		   (lambda () (get-a-value fnc-library proc)))))
      (if (symbol? result) `(,result) '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal (helper) procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-env (the-environment))

;;; Binds name to (list) proc, as an executable procedure 
(define (bind-proc name proc)
  (environment-define the-env 
		      name 
		      (lambda () (eval proc the-env))))

;;; Converts a (list) proc to a path of predicates suitable for the library trie
(define (proc->path proc)
  (if (null? proc)
      '()
      (let ((first-elem (car proc))
	    (rest (cdr proc)))
	(let ((elem-test (lambda (x) (eq? `,x first-elem))))
	  (append `(,elem-test) (proc->path (cdr proc)))))))

; --------------------------------

; ... testing library ...

(add-to-library '123-test '(+ 1 2 3))
(find-in-library '(+ 1 2 3))          ; -> |123-test|
(find-in-library '(+ 1 2 4))          ; -> ()

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




















