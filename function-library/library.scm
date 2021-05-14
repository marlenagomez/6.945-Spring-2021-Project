#|
The Library: Main Point of Entry

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 03 May 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

(load "function-library/trie-function-library")
(load "function-library/basic-function-library")

;;; Redundant Library

(define (intersection . lists)
  (cond ((= 0 (length lists)) '())
        ((= 1 (length lists)) (car lists))
        (else
         (let ((output (filter (lambda (elem)
				 (every (lambda (ls) (member elem ls)) (cdr lists)))
			       (car lists))))
	   output))))

(define (union . lists)
  (cond ((= 0 (length lists)) '())
	((= 1 (length lists)) (car lists))
	(else
	 (let ((output '()))
	   (for-each (lambda (l)
		       (for-each (lambda (elem)
				   (if (not (member elem output))
				       (set! output (append output (list elem)))))
				 l))
		     (filter (lambda (l) (> (length l) 0)) lists))
	   output))))

(define (make-redundant setup)
  
  (define (add type implementation)
    ((implementations 'put!) type (cons implementation '())))

  (define (add-tags type sticky-notes)
    (if ((implementations 'has?) type)
	((implementations 'put!) type (append ((implementations 'get) type)
					      sticky-notes))))
  
  (define (get-implementations)
    (map (lambda (key) (car ((implementations 'get) key))) 
	 ((implementations 'get-keys))))
  
  (define (get-implementation type)
    (cadr ((implementations 'get) type)))
  
  (define (get-sticky-notes type)
    (cddr ((implementations 'get) type)))
  
  (define (filter-by-sticky-note note)
    (filter (lambda (type) (memv tag (get-sticky-notes note))) ((implementations 'get-keys))))
  
  (define (combine-results operation combiner)
    (apply combiner (map-op operation)))
  
  (define (map-op operation)
    (map operation (get-implementations)))
  
  ;; setup the implementations and the message receiver
  (define implementations (make-alist-store eq?))       ; (type -> (implementation . sticky-notes))
  (for-each (lambda (i) ((implementations 'put!) (car i) (cons (cdr i) '()))) setup)

  (lambda (arg)
    (case arg
      ((is-redundant?) #t)
      ((map-op) map-op)
      ((combine-results) combine-results)
      ((add) add)
      ((add-tags) add-tags)
      ((get-implementations) get-implementations)
      ((get-implementation) get-implementation)
      ((get-tags) get-tags)
      ((filter-by-tag) filter-by-tag)
      (else #f))))

(define (setup-redundant) (make-redundant (list (cons 'trie (make-trie-library)) 
						(cons 'alist (make-alist-library))))) ; can hold any # of libraries

(define (is-redundant? library)
  (and (procedure? library)
       (library 'is-redundant?))) ; TODO: make more robust
		 
;;; Handles setup of the library

(define (setup-library #!optional type)
  (if (default-object? type)
      (setup-redundant)
      (case type
	((trie) (make-trie-library))
	((alist) (make-alist-library))
	(else (error "Unknown library type" type)))))

;;; Generic procedures for interfacing with the library

;;; --- adding to the library ---

(define (default-add-to library name proc) library)

(define add-to 
  (simple-generic-procedure 'add-to 3 default-add-to))

;; coderef: alist
(define-generic-procedure-handler add-to
  (match-args alist-library? symbol? list?)
  (lambda (library name proc) (add-to-alist library name proc)))

;; coderef: trie
(define-generic-procedure-handler add-to
  (match-args trie-library? symbol? list?)
  (lambda (library name proc) (add-to-trie library name proc)))

;; coderef: redundant
(define-generic-procedure-handler add-to
  (match-args is-redundant? symbol? list?)
  (lambda (library name proc) 
    ((library 'map-op) (lambda (l) (add-to l name proc)))))

;;; --- adding locally to the library ---

(define (default-add-local-to library name proc) library)

(define add-local-to
  (simple-generic-procedure 'add-local-to 3 default-add-local-to))

;; coderef: alist
(define-generic-procedure-handler add-local-to
  (match-args alist-library? symbol? list?)
  (lambda (library name proc) (add-local-to-alist library name proc)))

;; coderef: trie
(define-generic-procedure-handler add-local-to
  (match-args trie-library? symbol? list?)
  (lambda (library name proc) (add-local-to-trie library name proc)))

;; coderef: redundant
(define-generic-procedure-handler add-local-to
  (match-args is-redundant? symbol? list?)
  (lambda (library name proc)
    ((library 'map-op) (lambda (l) (add-local-to l name proc)))))

;;; --- searching in the library ---

(define (default-find-in library proc) '())

(define find-in
  (simple-generic-procedure 'find-in 2 default-find-in))

;; coderef: alist
(define-generic-procedure-handler find-in
  (match-args alist-library? list?)
  (lambda (library proc) (find-in-alist library proc)))

;; coderef: trie
(define-generic-procedure-handler find-in
  (match-args trie-library? list?)
  (lambda (library proc) (find-in-trie library proc)))

;; coderef: redundant
(define-generic-procedure-handler find-in
  (match-args is-redundant? list?)
  (lambda (library proc)
    ((library 'combine-results) (lambda (l) (find-in l proc)) intersection)))

;;; --- lookup a name in the library ---

(define (default-lookup library name) #f)

(define lookup
  (simple-generic-procedure 'lookup 2 default-lookup))

;; coderef: alist
(define-generic-procedure-handler lookup
  (match-args alist-library? symbol?)
  (lambda (library name)
    (lookup-in-alist library name)))

;; coderef: trie
(define-generic-procedure-handler lookup
  (match-args trie-library? symbol?)
  (lambda (library name)
    (lookup-in-trie library name)))

;; coderef: redundant
(define-generic-procedure-handler lookup
  (match-args is-redundant? symbol?)
  (lambda (library name)
    ;; find the first existing lookup result not equal to #f
    (find (lambda (x) x)
	  ((library 'map-op) (lambda (l) (lookup l name))))))

;;; --- running locally via the library ---

(define (default-run library name)
  (let ((proc (lookup library name)))
    (if proc
	(local-evaluation proc)
	(error "Procedure not in library:" name))))

(define run-locally 
  (simple-generic-procedure 'run-locally 2 default-run))

;;; --- removing from the library ---

(define (default-remove-from library name) library)

(define remove-from
  (simple-generic-procedure 'remove-from 2 default-remove-from))

;; coderef: alist
(define-generic-procedure-handler remove-from
  (match-args alist-library? symbol?)
  (lambda (library name) (remove-from-alist library name)))

;; coderef: trie
(define-generic-procedure-handler remove-from
  (match-args trie-library? symbol?)
  (lambda (library name) (remove-from-trie library name)))

;; coderef: redundant
(define-generic-procedure-handler remove-from
  (match-args is-redundant? symbol?)
  (lambda (library name) 
    ((library 'map-op) (lambda (l) (remove-from-trie l name)))))

;;; --- browsing the library ---

(define (default-browse library) library)

(define browse
  (simple-generic-procedure 'browse 1 default-browse))

;; coderef: alist
(define-generic-procedure-handler browse
  (match-args alist-library?)
  (lambda (library) (get-all-entries-in-alist library)))

;; coderef: trie
(define-generic-procedure-handler browse
  (match-args trie-library?)
  (lambda (library) (get-all-entries-in-trie library)))

;; coderef: redundant
(define-generic-procedure-handler browse
  (match-args is-redundant?)
  (lambda (library) 
    ((library 'combine-results) browse union)))





