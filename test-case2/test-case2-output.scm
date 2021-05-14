(define (expr-108 y x2 x0)
  (eq? #f (if (and (= (remainder y x2) x0) (> y x0)) #f #t)))

(define (expr-107 y x2 x0)
  (eq? #t (if (and (= (remainder y x2) x0) (> y x0)) #f #t)))

(define (expr-104 n t x)
  (* (sin (* 'n 't)) 'x))

(define (expr-103 n t y)
  (* (cos (* 'n 't)) 'y))

(define (expr-98 y x2 x0)
  (if (and (= (remainder y x2) x0)
           (> y x0))
      #f
      #t))

(define (expr-91 y x2 x0)
  (and (= (remainder y x2) x0)
       (> y x0)))

(define (expr-90 n t)
  (sin (* 'n 't)))

(define (expr-89 n t)
  (cos (* 'n 't)))

(define (expr-85 b a t)
  (+ (square (- (car b) a)) t))

(define (expr-74 + first-list)
  (/ (apply + first-list) (length first-list)))

(define (expr-73)
  ((first-list (car lists))))

(define (expr-72 n t)
  (* 'n 't))

(define (expr-67 new combining-arithmetics)
  (manage 'new 'combining-arithmetics))

(define (expr-68 m x3 r_0 x6)
  (* (expt m x3) (expt r_0 x6)))

(define (expr-65 t d x1)
  (/ t (- (length d) x1)))

(define (expr-64 b a)
  (square (- (car b) a)))

(define (expr-63)
  ((a (avg d))))

(define (expr-40 y x2 x0)
  (= (remainder y x2) x0))

(define (expr-55 x1 lists)
  (= x1 (length lists)))

(define (expr-54 lists)
  (first-list (car lists)))

(define (expr-53 gm dt x3 p_phi_0)
  (* 1/3 gm (expt dt x3) p_phi_0))

(define (expr-52 d x1)
  (= (length d) x1))

(define (expr-51 d x1)
  (- (length d) x1))

(define (expr-50 b a)
  (- (car b) a))

(define (expr-46 b x0)
  (> (length b) x0))

(define (expr-49 d)
  (a (avg d)))

(define (expr-36 y)
  (upper-bound y))

(define (expr-35 y)
  (lower-bound y))

(define (expr-33 upper-bound cdr)
  (define upper-bound
    cdr))

(define (expr-32 y x0)
  (> y x0))

(define (expr-4 y x2)
  (remainder y x2))

(define (expr-15 lists)
  (cdr lists))

(define (expr-14 first-list)
  (length first-list))

(define (expr-29 + first-list)
  (apply + first-list))

(define (expr-16 lists)
  (car lists))

(define (expr-25 x)
  'x)

(define (expr-26 r_0 x6)
  (expt r_0 x6))

(define (expr-18 d)
  (std d))

(define (expr-21 d)
  (variance d))

(define (expr-12 d)
  (avg d))

(define (expr-9 i x1)
  (+ i x1))

(define (expr-6 n)
  (smallest-divisor n))

(define (expr-8 a b)
  (divides? a b))

(define (expr-1 test-divisor)
  (square test-divisor))

((define (expr-1 x)
   (* x x))
 (define (even? n)
   (expr-40 n 2 0))
 (define (prime? n)
   (= (expr-6 n) n))
 (define (find-divisor n test-divisor)
   (cond ((> (expr-1 test-divisor) n) n)
         ((expr-8 test-divisor n) test-divisor)
         (else (find-divisor n (expr-9 test-divisor 1)))))
 (define (expr-8 a b)
   (= (remainder b a) 0))
 (define (expr-6 n)
   (find-divisor n 2))
 (define (expr-12 a)
   (define (avg-iter b t i)
     (cond ((expr-46 b 0)
            (avg-iter (expr-15 b) (+ (expr-16 b) t) (expr-9 i 1)))
           (else (/ t (expr-14 a)))))
   (avg-iter a 0. 0.))
 (define (expr-18 d)
   (let (expr-63)
     (define (std-iter b t)
       (if (expr-46 b 0)
           (std-iter (expr-15 b) (expr-85 b a t))
           (sqrt (expr-65 t d 1))))
     (if (expr-52 d 1)
         0.
         (std-iter d 0.))))
 (define (expr-21 d)
   (let (expr-63)
     (define (variance-iter b t)
       (if (expr-46 b 0)
           (variance-iter (expr-15 b) (expr-85 b a t))
           (expr-65 t d 1)))
     (if (expr-52 d 1)
         0.
         (variance-iter d 0.))))
 (define (expr-21 d)
   (* (expr-18 d) (expr-18 d)))
 (load "../sdf/manager/load")
 (expr-67 new term)
 (algebra-2
  '(+
    (+
     (/ (expr-53 gm dt 3 p_r_0) (expr-68 m 2 r_0 3))
     (/
      (* -1/2 (expr-26 dt 3) (expr-26 p_phi_0 2) p_r_0)
      (expr-68 m 3 r_0 4)))
    (+
     (/ (* (expr-26 dt 3) p_phi_0 (expr-26 p_r_0 2)) (expr-68 m 3 r_0 4))
     (/ (expr-53 gm dt 3 p_phi_0) (expr-68 m 2 r_0 5))
     (/ (* -1/3 (expr-26 dt 3) (expr-26 p_phi_0 3)) (expr-68 m 3 r_0 6)))))
 (expr-67 new combining-arithmetics)
 (install-arithmetic! combined-arithmetic)
 (+ (expr-103 n t x) (expr-104 n t y))
 (- (expr-103 n t y) (expr-104 n t x))
 (define (get-highest-average lists)
   (let (expr-73)
     (if (expr-55 1 lists)
         (expr-74 + first-list)
         (max (expr-74 + first-list) (get-highest-average (expr-15 lists))))))
 (define (get-lowest-average lists)
   (let (expr-73)
     (if (expr-55 1 lists)
         (expr-74 + first-list)
         (min (expr-74 + first-list) (get-lowest-average (expr-15 lists))))))
 (define (even-sum? x y)
   (if (or (and (expr-107 x 2 0))
           (expr-107 y 2 0))
       (and (expr-108 x 2 0))
       (expr-108 y 2 0)))
 (expr-33 make-interval cons)
 (expr-33 lower-bound car)
 (expr-33 upper-bound cdr)
 (define (add-interval x y)
   (make-interval (+ (expr-35 x) (expr-35 y)) (+ (expr-36 x) (expr-36 y))))
 (define (mul-interval x y)
   (let ((p1 (* (expr-35 x) (expr-35 y))) (p2 (* (expr-35 x) (expr-36 y)))
                                          (p3 (* (expr-36 x) (expr-35 y)))
                                          (p4 (* (expr-36 x) (expr-36 y))))
     (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4)))))


