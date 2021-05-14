(load "library-matcher.scm")
(load "cse.scm")
(load "cse-global.scm")

(define 




;; imports all code in a file as list
(define (read-doc file-path)
  (with-input-from-file file-path
    (lambda ()
      (let loop ((lines '())
		 (next-line (read)))
	(if (eof-object? next-line)
	    (reverse lines)
	    (loop (cons next-line lines)
		  (read)))))))


;tests

(run-cse (read-doc "test-case1.scm"))

(gjs/cselim (read-doc "test-case1.scm"))


;; writes to new file
(define (write-to-file x file-path)
  (let ((port (open-output-file file-path #t)))
    (let loop ((code x))
      (pp (car code) port #t)
      (newline port)
      (if (null? (cdr code))
	  (newline port)
	  (loop (cdr code))))
    (close-output-port port)))


;tests

(write-to-file '(+ 1 2) "awesome.scm")

(write-to-file (run-cse (read-doc "test-case1.scm")) "test-output6.scm")

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

(write-to-file (compress test-library (read-doc "test-case1.scm")) "test-output9.scm")




