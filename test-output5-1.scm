(define (multi-dot-prod list1 list2 list3 list4)
  (list (dot-product list1 list1) (dot-product list3 list4)))

(define (matrix-add m1 m2)
  (map (lambda (list1 list2) (map * list1 list2)) m1 m2))

(define (normalize vector)
  (sqrt (dot-product vector vector)))

(define (area-difference l1 h1 l2 h2)
  (determinant-2x2 l1 h1 l2 h2))

(define (volume-difference l1 h1 w1 l2 h2 w2)
  (- (* l1 h1 w1) (* l2 h2 w2)))

(define (det3 a b c d e f g h i)
  (+ (* a (determinant-2x2 e i f jh))
     (negate (* b (determinant-2x2 d i f g)))
     (* c (determinant-2x2 d h e g))))

(define (some-list-stats l1 l2 l3 l4 l5)
  (let ((avg1 (avg l1)) (avg2 (avg l2))
                        (avg3 (avg l3))
                        (avg4 (avg l4))
                        (avg5 (avg l5)))
    (let ((avg-ls (list avg1 avg2 avg3 avg4 avg5))
          (normal-lists
           (map (lambda (ls) (sqrt (dot-product ls ls)))
                (list l1 l2 l3 l4 l5))))
      (apply + avg-ls))))

(define (num-real-solutions a b c)
  (cond ((> 0 (descriminant b a c)) 0)
        ((< 0 (descriminant b a c)) 2)
        (else 1)))

(define (quadratic-solutions a b c)
  (values (/ (+ (negate b) (sqrt (descriminant b a c)) (* 2 a)))
          (/ (- (negate b) (sqrt (descriminant b a c)) (* 2 a)))))

(define (expected-value x p)
  (dot-product x p))

(define (variance x p)
  (let ((y (map (lambda (x0) (- x0 (dot-product x p))) x)))
    (let ((y-squared (map * y y)))
      (dot-product y-squared p))))

(define (vector-expected-value v p)
  (apply + (map (lambda (v0 p0) (lambda (v0-elem) (* v0-elem p0)) v0) v p)))


