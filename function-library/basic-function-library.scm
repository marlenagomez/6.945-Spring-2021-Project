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
  (make-alist-store eq?))

(define (alist-library? library)
  (alist? library))
(register-predicate! alist-library? 'alist-library?)

(define (add-to-alist library name proc)
  ((library 'put!) proc (cons name (gather-parameters proc))) ; add proc to library
  (bind-proc name proc)
  library)

(define (find-in-alist library proc)
  (let ((match (find (lambda (book) ((matcher book) proc)) 
		     ((library 'get-keys)))))
    (if (not match)
	'()
	(let ((library-value ((library 'get) match))
	      (dict (match:new-bindings (match:new-dict)
					((matcher match) proc))))
	  (let ((name (car library-value))
		(params (cdr library-value)))
	    (let ((matched-params (map-parameters params dict)))
	      `((,name ,@matched-params))))))))















