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

#|
(avg (list 0 1)) ; -> 0.5

(avg (list 1 2)) ; --> 1.5

(avg (list 0 2)) ; -> 1.0

(avg (list 0 1 2)) ; -> 1.0
|#

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

#|
(std (list 2)) ; -> 0.0

(std (list 0 1)) ; -> 0.707

(std (list 1 2)) ; -> 0.707

(std (list 0 2)) ; -> 1.414

(std (list 4 2 5 8 6)) ; -> 2.24
|#

; variance deviation of a list of numbers
(define (variance d)
  (let ((a (avg d)))
    (define (variance-iter b t)
      (if (> (length b) 0)
	  (variance-iter (cdr b) (+ (square (- (car b) a)) t))
	  (/ t (- (length d) 1))))
    (if (= (length d) 1)
	       0.0
	(variance-iter d 0.0))))

#|
(variance (list 1 2)) ; -> 0.5

(variance (list 0 2)) ; -> 2.0
|#

(define (variance d)
  (* (std d) (std d)))

#|
(variance (list 0 2)) ; -> 2.0
|#

(load "../sdf/manager/load")
(manage 'new 'term)

(algebra-2 '(+ (+ (/ (* 1/3 GM (expt dt 3) p_r_0) (* (expt m 2) (expt r_0 3)))
       (/ (* -1/2 (expt dt 3) (expt p_phi_0 2) p_r_0) (* (expt m 3) (expt r_0 4))))
    (+ (/ (* (expt dt 3) p_phi_0 (expt p_r_0 2)) (* (expt m 3) (expt r_0 4)))
       (/ (* 1/3 GM (expt dt 3) p_phi_0) (* (expt m 2) (expt r_0 5)))
       (/ (* -1/3 (expt dt 3) (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6))))))

(manage 'new 'combining-arithmetics)

(install-arithmetic! combined-arithmetic)

(+ (* (cos (* 'n 't)) 'x) (* (sin (* 'n 't)) 'y))
(- (* (cos (* 'n 't)) 'y) (* (sin (* 'n 't)) 'x))

(define (get-highest-average lists)
  (let ((first-list (car lists)))
    (if (= 1 (length lists))
        (/ (apply + first-list) (length first-list))
        (max (/ (apply + first-list) (length first-list)) (get-highest-average (cdr lists))))))

#|
(get-highest-average (list (list 1 2 3) (list 2 3 4))) ; -> 3
|#

(define (get-lowest-average lists)
  (let ((first-list (car lists)))
    (if (= 1 (length lists))
        (/ (apply + first-list) (length first-list))
        (min (/ (apply + first-list) (length first-list)) (get-lowest-average (cdr lists))))))

(define (even-sum? x y)
  (if
  (or
    (and
      (eq? #t (if (and (= (remainder x 2) 0) (> x 0))
        #f
        #t)))
    (eq? #t (if (and (= (remainder y 2) 0) (> y 0))
        #f
        #t)))
    (and (eq? #f (if (and (= (remainder x 2) 0) (> x 0))
        #f
        #t)))
    (eq? #f (if (and (= (remainder y 2) 0) (> y 0))
        #f
        #t))))


#|
(even-sum? 3 2) ; -> #f
|#



(define make-interval cons)

(define lower-bound car)

(define upper-bound cdr)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

#|
(add-interval (make-interval 1 1) (make-interval 2 2)) ; -> (3 . 3)
(mul-interval (make-interval 1 2) (make-interval 2 3)) ; -> (2 . 6)
|#
