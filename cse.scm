#|
Common Subexpression Eliminator

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 26 April 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#


;;;; Simple-minded common-subexpression eliminator.
;;;    GJS: 9 December 2005, 23 December 2019

;(load "function-library.scm")

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

    ;                  (pp "here")
                      (if (> (length let-variables) 0)
                        (pp "Common subexpression(s) found! add-to-library?")))))
                      ;(record-expression! expression-recorder
                      ;                    canonical-expression
                      ;                    new-bound-variables))))

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
      (if (> (length let-variables) 0)
        (pp "Common subexpression(s) found! add-to-library?")))))))
      ;(make-canonical-lets let-variables let-body))))



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

;  (if (> (length bindings) 0) ; add repeated expression to library
;      (add-to-library (intern (symbol->string (caar bindings)))
;		      (cadar bindings))
;  )

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
  (if (> (length (expression-recorder 'seen)) 0)
      (pp (cadar (expression-recorder 'seen))))
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

;;; testing ...

(pp (gjs/cselim '(+ (* x 3) (- x y) (* x 3))))

#|
; Value ->
(* x 3)
"Common subexpression(s) found! add-to-library?"
#!unspecific
;Unspecified return value
|#


;;; user inputs name for common subexpression to add to library
(add-to-library '3x '(* x 3)) ; -> Value: |3x|

;; test added to library
(find-in-library '(* x 3)) ; -> ((|3x|))
(pp |3x|) ; -> (lambda () (* x 3))


(pp (gjs/cselim
     '(lambda (x)
        (/ (+ (* x 3) (- y z) (- x y) (* x 3))
           (- y z)))))

#|
; Value ->
(* x 3)
(- y z)
"Common subexpression(s) found! add-to-library?"
#!unspecific
;Unspecified return value
|#

(add-to-library 'y-z '(- y z)) ; -> y-z


(find-in-library '(- y z)) ; -> ((y-z))


(find-in-library '(* x 3)) ; -> ((|3x|))


(pp (gjs/cselim
     '(let ((x 32) (y 5))
        (+ (* x 3) (- x y) (* x 3)))))

#|
; Value ->
(* x 3)
"Common subexpression(s) found! add-to-library?"
#!unspecific
;Unspecified return value
|#

(find-in-library '(* x 3)) ; -> ((|3x|))


|#
