(cd "..")

(load "library-matcher.scm")
(load "cse.scm")
(load "cse-global.scm")

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

;; doesn't work
(null? (gjs/cselim '((+ 1 2) (define (simple-add a b)
 (+ a b)) (define (lambda-add a b) (lambda (a b) (+ a b))))
))

(gjs/cselim '((+ 1 2) (+ 1 2)))

(run-cse (read-doc "~/6.945/project/6.945-Spring-2021-Project/test-case1.scm"))

(run-cse '(cond ((> b c) (/ b c))
		((= b c) (/ b c))))

(gjs/cselim '(cond ((> b c) (/ b c))
		   ((= b c) (/ b c))
		   ((> b c) 'what)))

(gjs/cselim
     '(lambda (x)
        (/ (+ (* x 3) (- y z) (- x y) (* x 3))
           (- y z))))

(let ((tlist (list)))
  (append! tlist '(c d))
  tlist)

(define (cse file-path)
  (let ((function (read-doc file-path)))
    (cons (gjs/cselim function) (run-cse function))))

(cse "test-case1.scm")


;;name-prompt asks if the user wants to name a function and add to library


;;refactor takes name-prompt and replace flag to run add-to-library
;;output should be list


(define (read-file file-path)
  (with-input-from-file file-path
    (lambda ()
      (read))))

(read-file "~/6.945/project/6.945-Spring-2021-Project/test-case1/test-case1.scm")
