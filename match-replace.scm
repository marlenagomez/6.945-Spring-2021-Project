(load "~/6.945/sdf/manager/load.scm")
(manage 'new 'term)
(manage 'add 'design)
(manage 'add 'unification)
(manage 'add 'generic-procedures)

 

;;; inputs: function in list format
;;;         matching code from trie with (lib-function-name, unknowns, function body)
;;; need to amend above format



;; apply run-matcher to each component of function and library function body
;; if match, replace with (lib-function-name resolved-unknowns)
;; if #f, continue with existing value
(define (apply-run-matcher function-list lib-function)
  (define lib-function-body (caddr lib-function)) 
    (let ((resolved-unknowns (run-matcher 
			      (match:compile-pattern lib-function-body) 
			      function-list
			      match:bindings)))
      (cond ((not (list? function-list)) function-list)
	    ((list? resolved-unknowns) (resolved-function
					resolved-unknowns
					lib-function))
	    ((eqv? resolved-unknowns #f)
	     (map (lambda (exp) (apply-run-matcher exp lib-function)) function-list))
	    (else function-list))))


(list? (run-matcher (match:compile-pattern '((1 ((? b) 2 1) 11 6 7)))
		    test-list match:bindings))

;; test cases

(define test-list '(1 (3 2 1) 11 6 7))


(apply-run-matcher test-list (list '(name) '(vars) '(1 ((? b) 2 1) 11 6 7)))
(cddr (list '(name) '(vars) '(1 ((? b) 2 1) 11 6 7)))
; whole expression matched, returns (name 3)


(apply-run-matcher (list 2 1 (list 3 2 1)) (list '(name) '(vars) '((? b) 2 1)))
; part expression matched, returns (2 1 (name 3))

(apply-run-matcher 3 (list 1 2 3))
; 



;; create function from resolved-unknowns and function name
(define (resolved-function resolved-unknowns lib-function)
  (define lib-function-name (caar lib-function))
  (map 
   (lambda (x) 
     (cadr x)) (cons (list 0 lib-function-name 0) resolved-unknowns)))

; test cases

(resolved-function (run-matcher (match:compile-pattern '(1 ((? b) 2 1) 11 6 7)) 
		    test-list match:bindings) '((name) (vars) (body))
; Value: (name 3)




;; alternative: make this into a generic function to deal with lists, lambda, if

(define list-matcher
  (simple-generic-procedure 'pred? 1 #f))

(define (list-if? x)
  (eqv? (car x) 'if))

(register-predicate! list-if? 'list-if)

(define (list-lambda? x)
  (eqv? (car x) 'lambda))

(register-predicate! list-lambda? 'list-lambda)

(define (list-cond? x)
  (eqv? (car x) 'cond))

(register-predicate! list-cond? 'list-cond)

(define (list-procedure? x)
  (eqv? (car x) 'define))

(register-predicate! list-procedure? 'list-procedure)

(define (list-let? x)
  (eqv? (car x) 'let))

(register-predicate! list-let? 'list-let)


(define-generic-procedure-handler list-matcher
  (match-args list-if?)
  (apply-run-matcher 


