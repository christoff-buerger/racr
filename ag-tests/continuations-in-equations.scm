; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr))

(define cie-spec (create-specification))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          Language Specification                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-specification
 cie-spec
 
 (ast-rule 'S->NonLocalContinuation)
 (ast-rule 'NonLocalContinuation->A-B)
 (ast-rule 'A->)
 (ast-rule 'B->)
 
 (compile-ast-specifications 'S)
 
 (ag-rule
  non-local
  (S
   0
   (lambda (n c)
     (att-value 'non-local (ast-child 1 n) c)))
  (NonLocalContinuation
   0
   (lambda (n c)
     (att-value 'non-local (ast-child 1 n) c)))
  (NonLocalContinuation
   1
   (lambda (n c)
     (+ (att-value 'non-local (ast-child 2 (ast-parent n)) c) 1)))
  (NonLocalContinuation
   2
   (lambda (n c)
     (if c
         (c 10)
         1))))
 
 (compile-ag-specifications))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                   Tests                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define run-tests
  (lambda ()
    (let ((ast
           (with-specification
            cie-spec
            (create-ast
             'S
             (list
              (create-ast
               'NonLocalContinuation
               (list
                (create-ast
                 'A
                 (list))
                (create-ast
                 'B
                 (list)))))))))
      (assert (= (att-value 'non-local ast #f) 2))
      (assert (=
               (call/cc
                (lambda (c)
                  (att-value 'non-local ast c)))
               10))
      (assert (= (att-value 'non-local ast #f) 2)) ; Cache hit
      (assert (=
               (+
                (call/cc
                 (lambda (c)
                   (att-value 'non-local ast c)))
                10)
               20)))))