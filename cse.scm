#|
Common Subexpression Eliminator
from scmutils/src/general/gjs-cse.scm

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 26 April 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#


;;;; Simple-minded common-subexpression eliminator.
;;;    GJS: 9 December 2005, 23 December 2019

(load "function-library")

(declare (usual-integrations))

(define (subst new old where)
  (cond ((eq? where old)
	 new)
	((not (pair? where))
	 where)
	(else
	 (cons (subst new old (car where))
	       (subst new old (cdr where))))))

(define (find-matching-item l pred)
 (if (null? l)
     #f
     (if (pred (car l))
	  (car l)
	  (find-matching-item (cdr l) pred))))

(define (gjs/cselim expression #!optional not-worth-subdividing?)
  (if (default-object? not-worth-subdividing?)
      (set! not-worth-subdividing? (lambda (expr) #f)))
  (let ((initial-expression-recorder
         (make-expression-recorder #f '())))
    (define (walk expression expression-recorder)
      (cond ((symbol? expression) expression)
            ((not-worth-subdividing? expression)
             (record-expression! expression-recorder expression '()))
            ((pair? expression)
             (case (car expression)
               ((quote) expression)
               ((lambda)
                (let* ((new-bound-variables (cadr expression))
                       (local-expression-recorder
                        (make-expression-recorder expression-recorder
                                                  new-bound-variables))
                       (canonicalized-body
                        (if (symbol? (caddr expression))
                            (record-expression! local-expression-recorder
                                                (caddr expression)
                                                '())
                            (walk (caddr expression)
                                  local-expression-recorder)))
                       (let-variables
                        (expressions-seen local-expression-recorder))
                       (lambda-body
                        (variable->expression local-expression-recorder
                                              canonicalized-body))
                       (canonical-expression
                        `(lambda ,new-bound-variables
                           ,(make-canonical-lets let-variables lambda-body))))
                  (record-expression! expression-recorder
                                      canonical-expression
                                      new-bound-variables)))
               ((let)
                (if (symbol? (cadr expression))
                    (error "Not implemented")
                    (let* ((new-bound-variables (map car (cadr expression)))
                           (values
                            (map (lambda (subexpression)
                                   (walk subexpression expression-recorder))
                                 (map cadr (cadr expression))))
                           (local-expression-recorder
                            (make-expression-recorder expression-recorder
                                                      new-bound-variables))
                           (canonicalized-body
                            (if (symbol? (caddr expression))
                                (record-expression! local-expression-recorder
                                                    (caddr expression)
                                                    '())
                                (walk (caddr expression)
                                      local-expression-recorder)))
                           (let-variables
                            (expressions-seen local-expression-recorder))
                           (let-body
                            (variable->expression local-expression-recorder
                                                  canonicalized-body))
                           (canonical-expression
                            (make-let-expression (cadr expression)
                                                 (make-canonical-lets let-variables
                                                                      let-body))))
                      (record-expression! expression-recorder
                                          canonical-expression
                                          new-bound-variables))))

               (else
                (let ((canonical-expression
                       (map (lambda (subexpression)
                              (walk subexpression expression-recorder))
                            expression)))
                  (record-expression! expression-recorder
                                      canonical-expression
                                      '())))))
            (else expression)))
    (let* ((canonical-expression
            (walk expression initial-expression-recorder))
           (let-variables
            (expressions-seen initial-expression-recorder))
           (let-body
            (variable->expression initial-expression-recorder
                                  canonical-expression)))
      (make-canonical-lets let-variables let-body))))



(define (make-let-expression bindings body)
  (let ((used-bindings
         (filter (lambda (b)
                   (occurs-in? (car b) body))
                 bindings)))
    (cond ((null? used-bindings) body)
          (else
           `(let ,used-bindings ,body)))))

(define (make-canonical-lets bindings body)
;  (pp bindings)
;  (pp (> (length bindings) 0))

  (if (> (length bindings) 0) ; add repeated expression to library
      (add-to-library (intern (symbol->string (caar bindings))) 
		      (cadar bindings))
  )

  (if (null? bindings)
      body
      (let-values
          (((independent dependent)
            (partition (lambda (binding)
                         (every (lambda (other-binding)
                                  (not (occurs-in? (car other-binding)
                                                   (cadr binding))))
                                bindings))
                       bindings)))
        (if (null? independent)
            (error "variables interdependent")
            (let ((inner (make-canonical-lets dependent body)))
              `(let ,independent
                 ,inner))))))

(define (make-expression-recorder parent-recorder bound-variables)
  (let ((local-expressions-seen '()))
    (lambda (message)
      (case message
        ((record!)
         (lambda (expression ignored-variables)
           (cond ((or (not parent-recorder)
                      (occurs-in? bound-variables expression))
                  (let ((vcell (assoc expression local-expressions-seen)))
                    (cond (vcell
                           ;; Increment reference count
                           (set-car! (cddr vcell) (fix:+ (caddr vcell) 1))
                           (cadr vcell))
                          (else
                           (let ((name (generate-uninterned-symbol "expr-")))
                             (set! local-expressions-seen
                                   (cons (list expression name 1)
                                         local-expressions-seen))
                             (set! bound-variables
                                   (cons name bound-variables))
                             name)))))
                 (else
                  ((parent-recorder 'record!) expression ignored-variables)))))
        ((seen)
         (let lp ((entries (reverse local-expressions-seen)) (results '()))
           (cond ((null? entries) (reverse results))
                 ((fix:= (caddar entries) 1)
                  (for-each
                   (lambda (entry)
                     (set-car! entry
                               (subst (caar entries) (cadar entries) (car entry))))
                   (cdr entries))
                  (lp (cdr entries) results))
                 (else
                  (lp (cdr entries)
                      (cons (list (cadar entries) (caar entries))
                            results))))))
        ((get-entry)
         (lambda (variable)
           (if (symbol? variable)
               (let ((entry
                      (find (lambda (entry) (eq? (cadr entry) variable))
                            local-expressions-seen)))
                 (if entry
                     entry
                     (if parent-recorder
                         ((parent-recorder 'get-entry) variable)
                         (error "Variable not present"))))
               (list variable))))
        (else
         (error "unknown message: expression-recorder" message))))))

(define (record-expression! expression-recorder expression ignored-variables)
  ((expression-recorder 'record!) expression ignored-variables))

(define (expressions-seen expression-recorder)
  (expression-recorder 'seen))

(define (variable->expression expression-recorder variable)
  (car ((expression-recorder 'get-entry) variable)))

(define (occurs-in? variables expression)
  (let lp ((expression expression))
    (cond ((pair? expression)
           (or (lp (car expression))
               (lp (cdr expression))))
          ((pair? variables)
           (memq expression variables))
          (else
           (eq? variables expression)))))

#|
(pp (gjs/cselim '(+ (* x 3) (- x y) (* x 3))))

; Value:
(let ((expr-1 (* x 3)))
  (+ expr-1 (- x y) expr-1))

;; test expr-1 added to library
(find-in-library '(* x 3))
; -> ((expr-13))
(pp expr-13)
; -> (lambda () (* x 3))

(pp (gjs/cselim
     '(lambda (x)
        (/ (+ (* x 3) (- y z) (- x y) (* x 3))
           (- y z)))))

; Value:
(let ((expr-5 (- y z)))
  (lambda (x)
    (let ((expr-4 (* x 3)))
      (/ (+ expr-4 expr-5 (- x y) expr-4) expr-5))))

(find-in-library '(- y z))
("returned match-dict" (dict ($value expr-5 ?)))
;Value: (#[uninterned-symbol 14 expr-5])

(find-in-library '(* x 3))
("returned match-dict" (dict ($value expr-4 ?)))
;Value: (#[uninterned-symbol 15 expr-4])

(pp (gjs/cselim
     '(let ((x 32) (y 5))
        (+ (* x 3) (- x y) (* x 3)))))

; Value:
(let ((x 32) (y 5))
  (let ((expr-10 (* x 3)))
    (+ expr-10 (- x y) expr-10)))

(find-in-library '(* x 3))
("returned match-dict" (dict ($value expr-10 ?)))
;Value: (#[uninterned-symbol 16 expr-10])


;;; this test case nor working because fringe-smaller-than?
(pp
 (gjs/cselim
  '(up
    (+ (/ (* 1/3 GM (expt dt 3) p_r_0) (* (expt m 2) (expt r_0 3)))
       (/ (* -1/2 (expt dt 3) (expt p_phi_0 2) p_r_0) (* (expt m 3) (expt r_0 4))))
    (+ (/ (* (expt dt 3) p_phi_0 (expt p_r_0 2)) (* (expt m 3) (expt r_0 4)))
       (/ (* 1/3 GM (expt dt 3) p_phi_0) (* (expt m 2) (expt r_0 5)))
       (/ (* -1/3 (expt dt 3) (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6)))))
  (fringe-smaller-than? 7)))
(let ((G44125 (expt dt 3)) (G44128 (* (expt m 3) (expt r_0 4))))
  (up
   (+ (/ (* 1/3 GM (expt dt 3) p_r_0) (* (expt m 2) (expt r_0 3)))
      (/ (* -1/2 G44125 (expt p_phi_0 2) p_r_0) G44128))
   (+ (/ (* G44125 p_phi_0 (expt p_r_0 2)) G44128)
      (/ (* 1/3 GM (expt dt 3) p_phi_0) (* (expt m 2) (expt r_0 5)))
      (/ (* -1/3 G44125 (expt p_phi_0 3)) (* (expt m 3) (expt r_0 6))))))
|#
