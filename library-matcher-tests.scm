;;;; Tests for the Library Matcher (library-matcher.scm)

;;; ----- loading dependencies -----

(ge user-initial-environment)
(load "library-matcher")

;;; ----- testing... -----

(define library (setup-library))
(trie-library? library)                
(alist-library? library)
(is-redundant? library)

(browse library) ; -> ()

(lookup library 'does-not-exist) ; -> #f

(add-to library 'expr-10 '(* x 3))
(match-in library '(* x 3))       ; -> (((* x 3) (expr-10)))
(pp expr-10)                      ; -> (lambda () (* x 3))
(compress library '(* x 3))       ; -> (expr-10)
(compress library '(+ (* x 3) (* x 3) (* x 3) (* x 3))) ; -> (+ (expr-10) (expr-10) (expr-10) (expr-10))

(add-to library '1+2+3 '(+ 1 2 3))
(match-in library '(+ 1 2 3))                     ; -> (((+ 1 2 3) (|1+2+3|)))
(match-in library '(let ((x (+ 1 2 3))) (pp x)))  ; -> (((+ 1 2 3) (|1+2+3|)))

(1+2+3) ; -> 6

(mod10 2) ; -> Unbound variable: mod10
(add-to library 'mod10 `(modulo (? x ,symbol?) 10))
(match-in library '(define (x y z)
                     (+ (modulo x 10) (modulo y 10) (modulo z 10))))   ; -> (((modulo x 10) (mod10 x)) ((modulo y 10) (mod10 y)) ((modulo z 10) (mod10 z))\)

(compress library '(define (x y z)
                     (+ (modulo x 10) (modulo y 10) (modulo z 10))))
; -> (define (x y z) (+ (mod10 x) (mod10 y) (mod10 z)))

(mod10 12) ; ->  2

(add-to library 'add-c-map `(map (lambda (l) (+ l (? x ,number?))) (? y ,list?)))
(match-in library '(define (a b c d e f)
                     (append (map (lambda (l) (+ l 4)) (a b))
                             (map (lambda (l) (+ l 5)) (c d))
                             (map (lambda (l) (+ l 6)) (e f)))))
#|
(((map (lambda (l) (+ l 4)) (a b)) (add-c-map 4 (a b)))
 ((map (lambda (l) (+ l 5)) (c d)) (add-c-map 5 (c d)))
 ((map (lambda (l) (+ l 6)) (e f)) (add-c-map 6 (e f))))
|#

(add-c-map 3 '(1 2 3)) ; -> (4 5 6)

(compress library '(define (a b c d e f)
                     (append (map (lambda (l) (+ l 4)) (a b c))
                             (map (lambda (l) (+ l 5)) (c d))
                             (map (lambda (l) (+ l 6)) (e f)))))
#|
(define (a b c d e f)
  (append (add-c-map 4 (a b c))
          (add-c-map 5 (c d))
          (add-c-map 6 (e f))))
|#

(compress library '(define (a b c d e f)
                     (append (map (lambda (l) (+ l 4)) (a b c))
                             (map (lambda (m) (modulo m 10)) (d e f)))))
#|
(define (a b c d e f)
  (append (add-c-map 4 (a b c))
          (map (lambda (m) (mod10 m)) (d e f))))
|#

(remove-from library 'add-c-map) ; -> TODO: not working rn

(compress library '(map (lambda (l) (+ l 4)) (a b c d e f)))
; ->  (add-c-map 4 (a b c d e f))

(add-c-map 10 '(1 2)) ; -> (11 12)

((run-locally library 'add-c-map) 4 '(3 2 1)) ; -> (7 6 5)

(browse library)

(lookup library '1+2+3)
(lookup library 'mod10)
(lookup library 'add-c-map)






