(+ 1 2)

(define (simple-add a b)
  (expr-3 a b))

(define (lambda-add a b)
  (lambda (a b)
    (expr-3 a b)))


(define (expr-3 a b)
  (+ a b))

((+ 1 2) (define (simple-add a b) (expr-3 a b))
         (define (lambda-add a b) (lambda (a b) (expr-3 a b))))


