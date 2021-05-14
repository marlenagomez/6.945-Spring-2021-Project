;;;; End-to-end test run of the program refactoring system

(cd "../..")
(load "run.scm")

(write-to-file (run-cse (read-doc "tests/test-run5/test-case5.scm")) 
	       "tests/test-run5/test-output5.scm")

(define my-library (setup-library))

(add-to my-library 
	'dot-product
	`(apply + (map * (? l1 ,symbol?) (? l2 ,symbol?))))

(add-to my-library
	'determinant-2x2
	`(- (* (? a ,symbol?) (? d ,symbol?))
	    (* (? b ,symbol?) (? c ,symbol?))))

(add-to my-library
	'avg
	`(/ (map * (? l1 ,symbol?)) (map + (? l1 ,symbol?))))

(add-to my-library
	'descriminant
	`(- (* (? b ,symbol?) (? b ,symbol?)) 
	    (* 4 (? a ,symbol?) (? c ,symbol?))))

(browse my-library)
;Value: (descriminant avg determinant-2x2 dot-product)

(write-to-file (compress my-library (read-doc "tests/test-run5/test-case5.scm"))
	       "tests/test-run5/test-output5-1.scm")
