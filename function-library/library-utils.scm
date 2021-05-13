#|
Library Utils

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 03 May 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

;;; binding / unbinging in the-environment

(define the-env (the-environment))

(define (bind-proc name proc)
  (environment-define the-env
                      name
                      (eval (proc->executable proc) the-env)))

(define (unbind-proc name)
  (unbind-variable the-env name))

(define (get-executable sym)
  (if (environment-bound? the-env sym)
      (environment-lookup the-env sym)
      #f))

(define (local-evaluation proc)
  (eval (proc->executable proc) the-env))

;;; alist-store support

(define (make-tagged-alist-store key=?)
  (cons 'alist-store (make-alist-store key=?)))

(define (alist-store? object) (and (pair? object) 
				   (eq? (car object) 'alist-store)))

(define (get-store alist-store) (cdr alist-store))

(define (add-to! store key value) (((get-store store) 'put!) key value))
(define (get key store) (((get-store store) 'get) key))
(define (has? store key) (((get-store store) 'has?) key))
(define (remove-from! store key) (del-assv! key (get-store store)))
(define (get-keys-from store) ((get-store store) 'get-keys))

;;; converts a match-formatted list to an executable lambda procedure
;;; by gathering the unknowns as parameters and regularizing the procedure
(define (proc->executable proc)
  `(lambda ,(gather-parameters proc) ,(regularize proc)))

;;; reformatted a procedure s.t. any nested lists are flattened
(define (flatten-proc proc)
  (apply append (map (lambda (elem)
                       (if (and (list? elem) (not (is-parameter? elem)))
                           (flatten-proc elem)
                           `(,elem)))
                     proc)))

;;; gathers the unknowns into a list
(define (gather-parameters proc)
  (fold (lambda (x curr-list) 
	  (if (not (memv x curr-list)) 
	      (append curr-list `(,x))
	      curr-list))
	'()
	(map param-name (filter is-parameter? (flatten-proc proc)))))

;;; converts unknowns into symbols
;;; e.g. (modulo (? x ,number?) 10) becomes (modulo x 10)
(define (regularize proc)
  (map (lambda (elem)
         (cond ((is-parameter? elem) (param-name elem))
               ((list? elem) (regularize elem))
               (else elem)))
       proc))

;;; handles procedure elements of the form (? symbol ,predicate?)

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
; ... testing...

(flatten-proc '(+ 1 2 3))
; -> (+ 1 2 3)
(flatten-proc `(modulo (? x ,symbol?) 3))
; -> (modulo (? x #[compiled-procedure 22 ("symbol" #x1) #x1c #x1059be2a4]) 3)
(flatten-proc `(map (lambda (l) (+ l (? x ,number?))) (? y ,list?)))
; -> (map lambda l + l (? x #[compiled-procedure 29 ("arith" #xa9) #x1c #x1057526d4]) (? y #[compiled-procedure 30 ("list" #x11) #x1c #x105a016fc]))

(proc->executable '(+ 1 2 3))                 
; -> (lambda () (+ 1 2 3))
(proc->executable `(modulo (? x ,symbol?) 3)) 
; -> (lambda (x) (modulo x 3))
(proc->executable `(map (lambda (l) (+ l (? x ,number?))) (? y ,list?)))
; -> (lambda (x y) (map (lambda (l) (+ l x)) y))
|#

;;; matching in the library utils

(define (safe-match-value-lookup param dict)
  (let ((lookup (match:lookup param dict)))
    (if (not lookup) #f (match:binding-value lookup))))

(define (map-parameters params match-dictionary)
  (map (lambda (p) (safe-match-value-lookup p match-dictionary)) params))




