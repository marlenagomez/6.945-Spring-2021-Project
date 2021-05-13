#|
Pattern Matcher
using the Function Library

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 25 April 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

;;; Dependencies
(load "function-library/library")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (chunkify scheme-code)
  (define (find-chunks code)
    (cond ((null? code) '())
          ((list? (car code)) (append `(,(car code))
                                      (find-chunks (car code))
                                      (find-chunks (cdr code))))
          (else (find-chunks (cdr code)))))
  (find-chunks `(,scheme-code)))

(define (match-in library scheme-code)
  ;; recursively check 'chunks'
  (define (check-all chunks)
    (if (null? chunks)
	'()
	(let ((search-result (find-in library (car chunks))))
	  (if (null? search-result)
	      (check-all (cdr chunks))
	      (append `(,(cons (car chunks) search-result))
		      (check-all (cdr chunks)))))))
  (check-all (chunkify scheme-code)))

(define (compress library scheme-code)
  
 ;(define replaced-record (make-alist-store eq?)) ; TODO: store record of what was replaced

  (define (walk code)
    (if (null? code)
	'()
	(map (lambda (elem)
	       (if (list? elem) 
		   (let ((search-result (find-in library elem)))
		     (if (null? search-result)
			 (walk elem)
			 (car search-result)))
		   elem))
	     code)))
  (car (walk `(,scheme-code))))


;-----------------------------------------------------------------------

#|
; ... testing chunkify ...

(chunkify '(+ 1 2 3))
; -> ((+ 1 2 3))

(chunkify '(+ 1 (* 2 3 4) 2))
; -> ((+ 1 (* 2 3 4) 2) (* 2 3 4))

(length (chunkify '(define (chunkify scheme-code)
		     (define (find-chunks code)
		       (cond ((null? code) '())
			     ((list? (car code)) (append `(,(car code))
							 (find-chunks (car code))
							 (find-chunks (cdr code))))
			     (else (find-chunks (cdr code)))))
		     (find-chunks `(,scheme-code)))))
; -> 28

#|
;Value: 
((define (chunkify scheme-code) 
   (define (find-chunks code) 
     (cond ((null? code) (quote ())) 
	   ((list? (car code)) (append (quasiquote ((unquote (car code)))) 
				       (find-chunks (car code)) 
				       (find-chunks (cdr code)))) 
	   (else (find-chunks (cdr code))))) 
   (find-chunks (quasiquote ((unquote scheme-code))))) 
 ;; chunk #2
 (chunkify scheme-code) 
 ;; chunk #3
 (define (find-chunks code) 
   (cond ((null? code) (quote ())) 
	 ((list? (car code)) (append (quasiquote ((unquote (car code)))) 
				     (find-chunks (car code)) 
				     (find-chunks (cdr code)))) 
	 (else (find-chunks (cdr code))))) 
 ;; chunk #3
 (find-chunks code)
 ;; chunk #4
 (cond ((null? code) (quote ())) 
       ((list? (car code)) (append (quasiquote ((unquote (car code)))) 
				   (find-chunks (car code)) 
				   (find-chunks (cdr code)))) 
       (else (find-chunks (cdr code)))) 
 ;; chunk #4
 ((null? code) (quote ())) 
 ;; chunk #5
 (null? code)
 ;; chunk #6
 (quote ()) 
 ;; chunk #7
 ()
 ;; chunk #8
 ((list? (car code)) (append (quasiquote ((unquote (car code)))) 
			     (find-chunks (car code)) 
			     (find-chunks (cdr code)))) 
 ;; chunk #9
 (list? (car code)) 
 ;; chunk #10
 (car code) 
 ;; chunk #11
 (append (quasiquote ((unquote (car code)))) 
	 (find-chunks (car code)) 
	 (find-chunks (cdr code))) 
 ;; chunk #12
 (quasiquote ((unquote (car code)))) 
 ;; chunk #13
 ((unquote (car code))) 
 ;; chunk #14
 (unquote (car code)) 
 ;; chunk #15
 (car code)
 ;; chunk #16
 (find-chunks (car code)) 
 ;; chunk #17
 (car code)
 ;; chunk #18
 (find-chunks (cdr code)) 
 ;; chunk #19
 (cdr code)
 ;; chunk #20
 (else (find-chunks (cdr code))) 
 ;; chunk #21
 (find-chunks (cdr code)) 
 ;; chunk #22
 (cdr code)
 ;; chunk #23
 (find-chunks (quasiquote ((unquote scheme-code)))) 
 ;; chunk #24
 (quasiquote ((unquote scheme-code))) 
 ;; chunk #25
 ((unquote scheme-code)) 
 ;; chunk #26
 (unquote scheme-code)
 )
|#
|#
