#|
Basic (List-based) Function Library

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 03 May 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

;;; dependencies

(load "library-utils")
(load "sdf/common/overrides")
(load "sdf/common/collections")
(load "sdf/common/predicates")
(load "sdf/common/generic-procedures")
(load "sdf/common/applicability")
(load "sdf/common/match-utils")
(load "sdf/design-of-the-matcher/matcher")

#|
fnc-library maps keys=expanded procedures to values=(executable name . parameter-list)
example: key=(modulo (? x ,symbol?) 10) --> value=(mod10 . (x))
|#
(define fnc-library (make-alist-store eq?))

(define (add-to-library name proc)
  ((fnc-library 'put!) proc (cons name (gather-parameters proc))) ; add proc to library
  (bind-proc name proc))

(define (find-in-library proc)
  (let ((match (find (lambda (book) ((matcher book) proc)) 
		     ((fnc-library 'get-keys)))))
    (if (not match)
	'()
	(let ((library-value ((fnc-library 'get) match))
	      (dict (match:new-bindings (match:new-dict)
					((matcher match) proc))))
	  (let ((name (car library-value))
		(params (cdr library-value)))
	    (let ((matched-params (map-parameters params dict)))
	      `((,name ,@matched-params))))))))

