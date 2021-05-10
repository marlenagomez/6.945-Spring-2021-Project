#|
The Library: Main Point of Entry

---------------------------------------------------------
Project: 6.945 | Program Refactoring
Date: 03 May 2021
Authors: Gabrielle Ecanow, Marlena Gomez, Katherine Liew
---------------------------------------------------------
|#

;;; Handles setup of the library
(define (setup-library type)
  (case type
    ((trie) (load "function-library/trie-function-library"))
    ((alist) (load "function-library/basic-function-library"))
    (else (error "Unknown library type" type)))
  (init-library))

