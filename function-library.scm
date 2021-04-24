#|
Function Library

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 24 April 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#


(load "sdf/manager/load")             ; not sure if needed...
(manage 'new 'generic-procedures)     ; loads common/trie.scm
                                      ; maybe: just (load "sdf/common") ?
(manage 'add 'generic-interpreter)    ; for define-variable!

(define fnc-library (make-trie))

;;;; User interface

(define (add-to-library name proc)) ; TODO

;;;; Internal (helper) procedures

(define the-env (the-environment))

(define (bind-proc name proc)
  (environment-define the-env name (lambda () (apply (car proc) (cdr proc)))))

(bind-proc 'test-name '(+ 1 2 3))

(test-name)

;;; Converts a (list) proc to a path of predicates suitable for the library trie
(define (proc->path proc)
  (if (null? proc)
      '()
      (let ((first-elem (car proc))
	    (rest (cdr proc)))
	(let ((elem-test (lambda (x) (eq? `,x first-elem))))
	  (append `(,elem-test) (proc->path (cdr proc)))))))

; ... testing ...

(define test-proc '(+ 1 2 3))

(define proc->path:test (proc->path test-proc))

(define name (intern-path-trie fnc-library proc->path:test))

(set-path-value! fnc-library proc->path:test 'name)

;;; demonstration: get-a-value given a (list) proc should return the (procedure) name
(get-a-value fnc-library '(+ 1 2 3)) 
; -> name



