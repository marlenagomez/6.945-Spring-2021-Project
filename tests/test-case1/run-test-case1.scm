(cd "../..")

(load "run.scm")

;tests

(run-cse (read-doc "tests/test-case1/test-case1.scm"))

(gjs/cselim (read-doc "tests/test-case1/test-case1.scm"))


;tests

(write-to-file '(+ 1 2) "tests/test-case1/awesome.scm")

(write-to-file (run-cse (read-doc "tests/test-case1/test-case1.scm")) 
	       "tests/test-case1/test-output6.scm")

#| writes to new file:
(+ 1 2)

(define (simple-add a b)
  (expr-3 a b))

(define (lambda-add a b)
  (lambda (a b)
    (expr-3 a b)))
|#

(define test-library (setup-library 'trie))

(add-to test-library `three `(+ 1 2))

(write-to-file (compress test-library (read-doc "tests/test-case1/test-case1.scm")) 
	       "tests/test-case1/test-output9.scm")

