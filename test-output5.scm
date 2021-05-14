(define (expr-24 c d h e g)
  (* c (- (* d h) (* e g))))

(define (expr-20 + * ls)
  (sqrt (apply + (map * ls ls))))

(define (expr-22 * l5 +)
  (/ (map * l5) (map + l5)))

(define (expr-21 d h e g)
  (- (* d h) (* e g)))

(define (expr-17 + * ls)
  (apply + (map * ls ls)))

(define (expr-13 l1 l2 l3 l4 l5)
  (list l1 l2 l3 l4 l5))

(define (expr-12 * ls)
  (map * ls ls))

(define (expr-8 e g)
  (* e g))

(define (expr-3 * vector)
  (map * vector vector))

((define (multi-dot-prod list1 list2 list3 list4)
   (list (expr-17 + * list1) (apply + (expr-12 #f #f))))
 (define (matrix-add m1 m2)
   (map (lambda (list1 list2) (expr-12 #f #f)) m1 m2))
 (define (normalize vector)
   (expr-20 + * vector))
 (define (area-difference l1 h1 l2 h2)
   (expr-21 l1 h1 l2 h2))
 (define (volume-difference l1 h1 w1 l2 h2 w2)
   (expr-21 l1 h1 l2 h2))
 (define (det3 a b c d e f g h i)
   (+ (expr-24 a e i f jh) (negate (expr-24 b d i f g)) (expr-24 c d h e g)))
 (define (some-list-stats l1 l2 l3 l4 l5)
   (let ((avg1 (expr-22 * l1 +)) (avg2 (expr-22 * l2 +))
                                 (avg3 (expr-22 * l3 +))
                                 (avg4 (expr-22 * l4 +))
                                 (avg5 (expr-22 * l5 +)))
     (let ((avg-ls (expr-13 avg1 avg2 avg3 avg4 avg5))
           (normal-lists
            (map (lambda (ls) (expr-20 + * ls)) (expr-13 l1 l2 l3 l4 l5))))
       (apply + avg-ls)))))


