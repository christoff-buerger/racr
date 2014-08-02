; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr core))

(define initialize-tests
  (lambda ()
    (with-specification
     (create-specification)
     
     (ast-rule 'S->NonLocalContinuation)
     (ast-rule 'NonLocalContinuation->A-B)
     (ast-rule 'A->)
     (ast-rule 'B->)
     (compile-ast-specifications 'S)
     
     (ag-rule
      non-local
      (S
       (lambda (n c)
         (att-value 'non-local (ast-child 'NonLocalContinuation n) c)))
      (NonLocalContinuation
       (lambda (n c)
         (att-value 'non-local (ast-child 'A n) c)))
      ((NonLocalContinuation A)
       (lambda (n c)
         (+ (att-value 'non-local (ast-sibling 'B n) c) 1)))
      ((NonLocalContinuation B)
       (lambda (n c)
         (if c
             (c 10)
             1))))
     (compile-ag-specifications)
     
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

(define run-tests
  (lambda ()
    (let ((ast (initialize-tests)))
      (assert (= (att-value 'non-local ast #f) 2))
      (assert (=
               (call/cc
                (lambda (c)
                  (att-value 'non-local ast c)))
               10))
      (assert (= (att-value 'non-local ast #f) 2))
      (assert (=
               (+
                (call/cc
                 (lambda (c)
                   (att-value 'non-local ast c)))
                10)
               20)))))

(run-tests)