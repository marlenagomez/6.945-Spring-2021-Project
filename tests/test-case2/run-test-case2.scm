(cd "../..")

(load "run.scm")

(write-to-file (run-cse (read-doc "tests/test-case2/test-case2.scm")) 
	       "tests/test-case2/test-case2-output.scm")
