;;; Github Test


(define (square x) (* x x))

(define (even? n) (= (remainder n 2) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

; average of a list of numbers
(define (avg a)
  (define (avg-iter b t i)
    (cond ((> (length b) 0)
	   (avg-iter (cdr b) (+ (car b) t) (+ i 1)))
	  (else
	   (/ t (length a)))))
  (avg-iter a 0.0 0.0))

(avg (list 0 1)) ; -> 0.5

(avg (list 1 2)) ; --> 1.5

(avg (list 0 2)) ; -> 1.0

(avg (list 0 1 2)) ; -> 1.0

; standard deviation of a list of numbers
(define (std d)
  (let ((a (avg d)))
    (define (std-iter b t)
      (if (> (length b) 0)
	  (std-iter (cdr b) (+ (square (- (car b) a)) t))
	  (sqrt (/ t (- (length d) 1)))))

    (if (= (length d) 1)
	0.0
	(std-iter d 0.0))))

(std (list 2)) ; -> 0.0

(std (list 0 1)) ; -> 0.707

(std (list 1 2)) ; -> 0.707

(std (list 0 2)) ; -> 1.414

(std (list 4 2 5 8 6)) ; -> 2.24
