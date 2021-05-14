
(define (expr-50 b x4 a c)
  (sqrt (- (* b b) (* x4 a c))))

(define (expr-42 c d h e g)
  (* c (- (* d h) (* e g))))

(define (expr-37 b x4 a c)
  (- (* b b) (* x4 a c)))

(define (expr-34 + * ls)
  (sqrt (apply + (map * ls ls))))

(define (expr-36 * l5 +)
  (/ (map * l5) (map + l5)))

(define (expr-35 d h e g)
  (- (* d h) (* e g)))

(define (expr-30 + * y-squared p)
  (apply + (map * y-squared p)))

(define (expr-8 v0-elem p0)
  (* v0-elem p0))

(define (expr-3 * y-squared p)
  (map * y-squared p))

(define (expr-22 x2 a)
  (* x2 a))

(define (expr-18 x4 a c)
  (* x4 a c))

(define (expr-21 b)
  (negate b))

(define (expr-14 l1 l2 l3 l4 l5)
  (list l1 l2 l3 l4 l5))

(define (expr-13 + l5)
  (map + l5))

(define (expr-10 l2 h2 w2)
  (* l2 h2 w2))

((define (multi-dot-prod list1 list2 list3 list4)
   (list (expr-30 + * list1 list1) (expr-30 + * list3 list4)))
 (define (matrix-add m1 m2)
   (map (lambda (list1 list2) (expr-3 * list1 list2)) m1 m2))
 (define (normalize vector)
   (expr-34 + * vector))
 (define (area-difference l1 h1 l2 h2)
   (expr-35 l1 h1 l2 h2))
 (define (volume-difference l1 h1 w1 l2 h2 w2)
   (expr-35 l1 h1 l2 h2))
 (define (det3 a b c d e f g h i)
   (+ (expr-42 a e i f jh) (negate (expr-42 b d i f g)) (expr-42 c d h e g)))
 (define (some-list-stats l1 l2 l3 l4 l5)
   (let ((avg1 (expr-36 * l1 +)) (avg2 (expr-36 * l2 +))
                                 (avg3 (expr-36 * l3 +))
                                 (avg4 (expr-36 * l4 +))
                                 (avg5 (expr-36 * l5 +)))
     (let ((avg-ls (expr-14 avg1 avg2 avg3 avg4 avg5))
           (normal-lists
            (map (lambda (ls) (expr-34 + * ls)) (expr-14 l1 l2 l3 l4 l5))))
       (apply + avg-ls))))
 (define (num-real-solutions a b c)
   (cond ((> 0 (expr-37 b 4 a c)) 0)
         ((< 0 (expr-37 b 4 a c)) 2)
         (else 1)))
 (define (quadratic-solutions a b c)
   (values (/ (+ (expr-21 b) (expr-50 b 4 a c) (expr-22 2 a)))
           (/ (- (expr-21 b) (expr-50 b 4 a c) (expr-22 2 a)))))
 (define (expected-value x p)
   (expr-30 + * x p))
 (define (variance x p)
   (let ((y (map (lambda (x0) (- x0 (expr-30 + * x p))) x)))
     (let ((y-squared (expr-3 * y y)))
       (expr-30 + * y-squared p))))
 (define (vector-expected-value v p)
   (apply
    +
    (map (lambda (v0 p0) (lambda (v0-elem) (expr-8 v0-elem p0)) v0) v p))))


