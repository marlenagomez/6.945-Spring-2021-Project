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
(load "function-library")      ; trie matching implementation
;(load "basic-function-library") ; basic matching implementation

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (chunkify scheme-code)
  (define (find-chunks code)
    (cond ((null? code) '())
          ((list? (car code)) (append `(,(car code))
                                      (find-chunks (car code))
                                      (find-chunks (cdr code))))
          (else (find-chunks (cdr code)))))
  (find-chunks `(,scheme-code)))

(define (match-in-library scheme-code)
  ;; recursively check 'chunks'
  (define (check-all chunks)
    (if (null? chunks)
	'()
	(let ((search-result (find-in-library (car chunks))))
	  (if (null? search-result)
	      (check-all (cdr chunks))
	      (append `(,(cons (car chunks) search-result))
		      (check-all (cdr chunks)))))))
  (check-all (chunkify scheme-code)))

(define (library-compress scheme-code)
  
 ;(define replaced-record (make-alist-store eq?)) ; TODO: store record of what was replaced

  (define (walk code)
    (if (null? code)
	'()
	(map (lambda (elem)
	       (if (list? elem) 
		   (let ((search-result (find-in-library elem)))
		     (if (null? search-result)
			 (walk elem)
			 (car search-result)))
		   elem))
	     code)))
  (car (walk `(,scheme-code))))


;------------------------------------------------------------------------
; ... testing ...

(add-to-library 'expr-10 '(* x 3))
(match-in-library '(* x 3))       ; -> (((* x 3) (expr-10)))
(pp expr-10)                      ; -> (lambda () (* x 3))
(library-compress '(* x 3))       ; -> (expr-10)
(library-compress '(+ (* x 3) (* x 3) (* x 3) (* x 3))) ; -> (+ (expr-10) (expr-10) (expr-10) (expr-10))

(add-to-library '1+2+3 '(+ 1 2 3))
(match-in-library '(+ 1 2 3))                     ; -> (((+ 1 2 3) (|1+2+3|)))
(match-in-library '(let ((x (+ 1 2 3))) (pp x)))  ; -> (((+ 1 2 3) (|1+2+3|)))

(1+2+3) ; -> 6

(mod10 2) ; -> Unbound variable: mod10
(add-to-library 'mod10 `(modulo (? x ,symbol?) 10))
(match-in-library '(define (x y z)
		     (+ (modulo x 10) (modulo y 10) (modulo z 10))))   ; -> (((modulo x 10) (mod10 x)) ((modulo y 10) (mod10 y)) ((modulo z 10) (mod10 z)))
(library-compress '(define (x y z)
		     (+ (modulo x 10) (modulo y 10) (modulo z 10))))
; -> (define (x y z) (+ (mod10 x) (mod10 y) (mod10 z)))

(mod10 2) ; ->  2

(add-to-library 'add-c-map `(map (lambda (l) (+ l (? x ,number?))) (? y ,list?)))
(match-in-library '(define (a b c d e f)
		     (append (map (lambda (l) (+ l 4)) (a b))
			     (map (lambda (l) (+ l 5)) (c d))
			     (map (lambda (l) (+ l 6)) (e f)))))
#|
(((map (lambda (l) (+ l 4)) (a b)) (add-c-map 4 (a b))) 
 ((map (lambda (l) (+ l 5)) (c d)) (add-c-map 5 (c d))) 
 ((map (lambda (l) (+ l 6)) (e f)) (add-c-map 6 (e f))))
|#

(add-constant-map 3 '(1 2 3)) ; -> (4 5 6)

(library-compress '(define (a b c d e f)
		     (append (map (lambda (l) (+ l 4)) (a b c))
			     (map (lambda (l) (+ l 5)) (c d))
			     (map (lambda (l) (+ l 6)) (e f)))))
#|
(define (a b c d e f) 
  (append (add-c-map 4 (a b c)) 
	  (add-c-map 5 (c d)) 
	  (add-c-map 6 (e f))))
|#

(library-compress '(define (a b c d e f)
		     (append (map (lambda (l) (+ l 4)) (a b c))
			     (map (lambda (m) (modulo m 10)) (d e f)))))
#|
(define (a b c d e f) 
  (append (add-c-map 4 (a b c)) 
	  (map (lambda (m) (mod10 m)) (d e f))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
