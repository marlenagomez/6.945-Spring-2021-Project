(cd ..)

(load "run.scm")

(write-to-file (run-cse (read-doc "test-case2/test.scm")) "test-case2/test_output.scm")
