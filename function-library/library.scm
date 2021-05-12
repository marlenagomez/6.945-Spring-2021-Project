#|
The Library: Main Point of Entry

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 03 May 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

(load "function-library/trie-function-library")
(load "function-library/basic-function-library")

;;; Handles setup of the library
(define (setup-library type)
  (case type
    ((trie) (make-trie-library))
    ((alist) (make-alist-library))
    (else (error "Unknown library type" type))))

;;; Generic procedures for interfacing with the library

;;; --- adding to the library ---

(define (default-add-to library name proc) library)

(define add-to 
  (simple-generic-procedure 'add-to 3 default-add-to))

;; coderef: alist
(define-generic-procedure-handler add-to
  (match-args alist-library? symbol? list?)
  (lambda (library name proc) (add-to-alist library name proc)))

;; coderef: trie
(define-generic-procedure-handler add-to
  (match-args trie-library? symbol? list?)
  (lambda (library name proc) (add-to-trie library name proc)))

(define add-local-to
  (simple-generic-procedure 'add-local-to 3 default-add-to))

;; coderef: alist
(define-generic-procedure-handler add-local-to
  (match-args alist-library? symbol? list?)
  (lambda (library name proc) (add-local-to-alist library name proc)))

;; coderef: trie
(define-generic-procedure-handler add-local-to
  (match-args trie-library? symbol? list?)
  (lambda (library name proc) (add-local-to-trie library name proc)))

;;; --- searching in the library ---

(define (default-find-in library proc) '())

(define find-in
  (simple-generic-procedure 'find-in 2 default-find-in))

;; coderef: alist
(define-generic-procedure-handler find-in
  (match-args alist-library? list?)
  (lambda (library proc) (find-in-alist library proc)))

;; coderef: trie
(define-generic-procedure-handler find-in
  (match-args trie-library? list?)
  (lambda (library proc) (find-in-trie library proc)))


;;; --- running locally via the library ---

(define (default-run library name) #f)

(define run-locally 
  (simple-generic-procedure 'run-locally 2 default-run))

;; coderef: alist
(define-generic-procedure-handler run-locally
  (match-args alist-library? symbol?)
  (lambda (library name) 
    (local-evaluation (lookup-in-alist library name))))

;; coderef: trie
(define-generic-procedure-handler run-locally
  (match-args trie-library? symbol?)
  (lambda (library name)
    (local-evaluation (lookup-in-trie library name))))

;;; --- removing from the library ---

(define (default-remove-from library name) library)

(define remove-from
  (simple-generic-procedure 'remove-from 2 default-remove-from))

;; coderef: alist
(define-generic-procedure-handler remove-from
  (match-args alist-library? symbol?)
  (lambda (library name) (remove-from-alist library name)))

;; coderef: trie
(define-generic-procedure-handler remove-from
  (match-args trie-library? symbol?)
  (lambda (library name) (remove-from-trie library name)))

