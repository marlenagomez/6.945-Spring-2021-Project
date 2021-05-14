;;;; run.scm
;;;; Handles reading and writing to .scm files
;;;; Imports the necessary dependencies for 
;;;; compressing the code in the loaded files

(load "function-library/library.scm")
(load "cse.scm")
(load "cse-global.scm")

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
