#|
Basic (List-based) Function Library

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 03 May 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

;;; dependencies

(load "function-library/library-utils")
(load "sdf/common/overrides")
(load "sdf/common/collections")
(load "sdf/common/predicates")
(load "sdf/common/generic-procedures")
(load "sdf/common/applicability")
(load "sdf/common/match-utils")
(load "sdf/design-of-the-matcher/matcher")


;;; fnc-library maps keys=expanded procedures to values=(executable name . parameter-list)
;;; example: key=(modulo (? x ,symbol?) 10) --> value=(mod10 . (x))

(define (make-alist-library)
  (make-tagged-alist-store eq?))

(define (alist-library? library)
  (alist-store? library))
(register-predicate! alist-library? 'alist-library?)

(define (add-to-alist library name proc)
  (add-to! library proc (cons name (gather-parameters proc)))
  (bind-proc name proc)
  library)

(define (add-local-to-alist library name proc)
  (add-to! library proc (cons name (gather-parameters proc)))
  library)

(define (find-in-alist library proc)
  (let ((match (find (lambda (book) ((matcher book) proc)) 
		     ((get-keys-from library)))))
    (if (not match)
	'()
	(let ((library-value (get match library))
	      (dict (match:new-bindings (match:new-dict)
					((matcher match) proc))))
	  (let ((name (car library-value))
		(params (cdr library-value)))
	    (let ((matched-params (map-parameters params dict)))
	      `((,name ,@matched-params))))))))

(define (lookup-in-alist library name)
  (find (lambda (key)
	  (eq? name (car (get key library))))
	(get-keys-from library)))

(define (remove-from-alist library name)
  (remove-from! library name)
  (unbind-proc name)
  library)













