#|
Function Library

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 24 April 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

;;; Dependencies

;;; trie matcher
(load "trie-matcher")

;;; sdf/efficient-generic/procedures/load-spec-trie
(load "sdf/common/arith")
(load "sdf/common/numeric-arith")
(load "sdf/combining-arithmetics/standard-arith")
(load "sdf/combining-arithmetics/function-variants")
(load "sdf/generic-procedures/generic-arith")
(load "sdf/common/trie")

;;; other
(load "sdf/common/match-utils")
(load "sdf/unification/unify")
(load "sdf/unification/type-resolver")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;; Appropriate arithmetic required
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

; perhaps unneeded...

(define full-generic-arithmetic
  (let ((g (make-generic-arithmetic make-simple-dispatch-store)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
				(symbolic-extender numeric-arithmetic))
    g))

(install-arithmetic! full-generic-arithmetic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fnc-library (make-trie))

; --- may be depreciated (first pass with predicates) ---
#|
(define (add-to-library name proc)
  (let ((proc-path (proc->path proc)))
    (set-path-value! fnc-library proc-path name) ; add proc to library
    (bind-proc name proc)))

(define (find-in-library proc)
  (let ((proc-path (proc->path proc)))
    (let ((result (ignore-errors 
		   (lambda () (get-a-value fnc-library proc)))))
      (if (symbol? result) `(,result) '()))))
|#

; --- alt. library functionality that uses the tmatcher ---

(define (add-to-library name proc)
  (define (tmatcher-proc->path proc)
    (map (lambda (elem) (lambda (args) elem)) proc))
  (set-path-value! fnc-library (tmatcher-proc->path) name)
  (bind-proc name proc))

(define (match-in-library proc)
  (let ((match-dict ((t:matcher fnc-library) proc)))
    (pp (list "returned match-dict" match-dict))
    ((match-dict 'get) '$value)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal (helper) procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-env (the-environment))

;;; Binds name to proc, as an executable procedure 
(define (bind-proc name proc)
  (environment-define the-env 
		      name 
		      (eval (proc->executable proc) the-env)))

;;; Converts a proc to a path of predicates suitable for the library trie
(define (proc->path proc)
  (map (lambda (elem) (get-predicate elem eq?)) proc))

(define (proc->executable proc)
  `(lambda ,(gather-parameters proc) ,(regularize proc)))

(define (gather-parameters proc)
  (map param-name (filter is-parameter? proc)))

(define (regularize proc)
  (map (lambda (elem) (if (is-parameter? elem) (param-name elem) elem))
       proc))

(define (is-parameter? elem)
  (and (list? elem)
       (eq? '? (car elem))))

(define (param-name parameter)
  (guarantee is-parameter? parameter)
  (cadr parameter))

(define (param-predicate parameter)
  (guarantee is-parameter? parameter)
  (caddr parameter))
 
(define (get-predicate proc-elem eq=?)
  (if (is-parameter? proc-elem) 
      (param-predicate proc-elem)
      (lambda (x) (eq=? `,x proc-elem))))

#|
(proc->path '(+ 1 2 3))
(proc->executable '(+ 1 2 3))                 ; -> (lambda () (+ 1 2 3))
(proc->path `(modulo (? x ,symbol?) 3))
(proc->executable `(modulo (? x ,symbol?) 3)) ; -> (lambda (x) (modulo x 3))
|#


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



















