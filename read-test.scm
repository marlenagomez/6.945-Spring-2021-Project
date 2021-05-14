(load "function-library.scm")
(load "cse.scm")
(load "function-library.scm")
(load "function-library.scm")

;; prints next object
(pp (with-input-from-file "~/6.945/sdf/common/arith.scm" 
  (lambda ()
    (read))))

;; prints first char in file
(with-input-from-file "~/6.945/sdf/common/arith.scm" 
  (lambda ()
    (read-char)))

;; identifies when first char is a #
(eqv? '#\# (with-input-from-file "~/6.945/sdf/common/arith.scm" 
	     (lambda ()
	       (read-char))))

;;works to import all code in a file as list
(define (read-doc file-path)
  (with-input-from-file file-path
    (lambda ()
      (let loop ((lines '())
		 (next-line (read)))
	(if (eof-object? next-line)
	    (reverse lines)
	    (loop (cons next-line lines)
		  (read)))))))

; tests

(read-doc "~/6.945/sdf/common/arith.scm")

(pp (read-doc "~/6.945/sdf/common/arith.scm"))

(read-doc "~/6.945/project/6.945-Spring-2021-Project/test-case1.scm")

;; refactor applies the cse 

(define (refactor file-path)
  (map (lambda (x) (name-prompt x) (gjs/cselim (read-doc file-path)))


;;name-prompt asks if the user wants to name a function and add to library

;;refactor takes name-prompt and replace flag to run add-to-library
;;output should be list


(define (read-file file-path)
  (with-input-from-file file-path
    (lambda ()
      (read))))

(read-file "~/6.945/project/6.945-Spring-2021-Project/test-case1.scm")
