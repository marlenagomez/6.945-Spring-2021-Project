
(define (multi-dot-prod list1 list2 list3 list4)
  (list (apply + (map * list1 list1))
	(apply + (map * list3 list4))))

(define (matrix-add m1 m2)
  (map (lambda (list1 list2)
	 (map * list1 list2))
       m1 m2))

(define (normalize vector)
  (sqrt (apply + (map * vector vector))))

(define (area-difference l1 h1 l2 h2)
  (- (* l1 h1) (* l2 h2)))

(define (volume-difference l1 h1 w1 l2 h2 w2)
  (- (* l1 h1 w1) (* l2 h2 w2)))

(define (det3 a b c d e f g h i)
  (+ (* a (- (* e i) (* f jh)))
     (negate (* b (- (* d i) (* f g))))
     (* c (- (* d h) (* e g)))))

(define (some-list-stats l1 l2 l3 l4 l5)
  (let ((avg1 (/ (map * l1) (map + l1)))
	(avg2 (/ (map * l2) (map + l2)))
	(avg3 (/ (map * l3) (map + l3)))
	(avg4 (/ (map * l4) (map + l4)))
	(avg5 (/ (map * l5) (map + l5))))
    (let ((avg-ls (list avg1 avg2 avg3 avg4 avg5))
	  (normal-lists (map (lambda (ls) (sqrt (apply + (map * ls ls)))) 
			     (list l1 l2 l3 l4 l5))))
      (apply + avg-ls))))

(define (num-real-solutions a b c)
  (cond ((> 0 (- (* b b) (* 4 a c))) 0)
	((< 0 (- (* b b) (* 4 a c))) 2)
	(else 1)))

(define (quadratic-solutions a b c)
  (values (/ (+ (negate b) (sqrt (- (* b b) (* 4 a c))) (* 2 a)))
	  (/ (- (negate b) (sqrt (- (* b b) (* 4 a c))) (* 2 a)))))

(define (expected-value X P)
  (apply + (map * X P)))

(define (variance X P)
  (let ((Y (map (lambda (x0) (- x0 (apply + (map * X P)))) X)))
    (let ((Y-squared (map * Y Y)))
      (apply + (map * Y-squared P)))))

(define (vector-expected-value V P)
  (apply + (map (lambda (v0 p0) (lambda (v0-elem) (* v0-elem p0)) v0) V P)))





