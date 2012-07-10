; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

; Unit tests for RACR's caching facilities. To test caching performance independent and without
; obfuscating RACR's code by printing cache hit messages or similar, we incorporate side effects
; into attribute equations which influence further attribute evaluation. If caching is enabled
; and working as intended, user calls to attributes should always yield the same result for the
; same attribute arguments. Without caching, attribute values should change.

#!r6rs

(import (rnrs) (racr))

; Non-circular attribute with side-effect. Each time its equation is evaluated it yields another value.
(define init-basic-test
  (lambda (cached?)
    (let* ((state #f))
      (with-specification
       (create-specification)
       ;;; Specify simple test language:
       (ast-rule 'S->)
       (compile-ast-specifications 'S)
       (ag-rule
        test-attribute
        (S
         cached?
         (lambda (n)
           (begin
             (set! state (not state))
             state))))
       (compile-ag-specifications)
       ;;; Return test AST:
       (create-ast 'S (list))))))

; Circular attribute with side-effect. Each time its equation is evaluated it yields another value.
(define init-basic-cycle-test
  (lambda (cached?)
    (let ((state 1))
      (with-specification
       (create-specification)
       ;;; Specify simple test language:
       (ast-rule 'S->A)
       (ast-rule 'A->)
       (compile-ast-specifications 'S)
       (ag-rule
        cyclic-att
        (S
         cached?
         (lambda (n)
           (let ((result (att-value 'cyclic-att (ast-child 1 n))))
             (set! state result)
             result))
         1
         =)
        ((S A)
         cached?
         (lambda (n)
           (let ((result (att-value 'cyclic-att (ast-parent n))))
             (if (> result 10)
                 result
                 (+ result state))))
         1
         =))
       (compile-ag-specifications)
       ;;; Return test AST:
       (create-ast 'S (list (create-ast 'A (list))))))))

(define init-fibonacci-numbers
  (lambda (cached?)
    (with-specification
     (create-specification)
     (ast-rule 'S->)
     (compile-ast-specifications 'S)
     (ag-rule
      fibonacci
      (S
       cached?
       (lambda (n num)
         (if (< num 2)
             num
             (+ (att-value 'fibonacci n (- num 1)) (att-value 'fibonacci n (- num 2)))))))
     (compile-ag-specifications)
     (create-ast 'S (list)))))

(define run-tests
  (lambda ()
    ; Test non-circular attribute with side-effect and caching:
    (let ((ast (init-basic-test #t)))
      (assert (att-value 'test-attribute ast))
      (assert (att-value 'test-attribute ast)))
    ; Test non-circular attribute with side-effect and without caching:
    (let ((ast (init-basic-test #f)))
      (assert (att-value 'test-attribute ast))
      (assert (not (att-value 'test-attribute ast)))
      (assert (att-value 'test-attribute ast))
      (assert (not (att-value 'test-attribute ast))))
    
    ; Test circular attribute with side-effect and caching:
    (let ((ast (init-basic-cycle-test #t)))
      (assert (= (att-value 'cyclic-att ast) 16))
      (assert (= (att-value 'cyclic-att ast) 16))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16))
      (assert (= (att-value 'cyclic-att ast) 16))
      (assert (= (att-value 'cyclic-att ast) 16)))
    ; ------Inverse Access Order------ ;
    (let ((ast (init-basic-cycle-test #t)))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16))
      (assert (= (att-value 'cyclic-att ast) 16))
      (assert (= (att-value 'cyclic-att ast) 16))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16)))
    
    ; Test circular attribute with side-effect and without caching:
    (let ((ast (init-basic-cycle-test #f)))
      (assert (= (att-value 'cyclic-att ast) 16))
      (assert (= (att-value 'cyclic-att ast) 17))
      (assert (= (att-value 'cyclic-att ast) 18))
      (assert (= (att-value 'cyclic-att ast) 19))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16))
      (assert (= (att-value 'cyclic-att ast) 17))
      (assert (= (att-value 'cyclic-att ast) 18))
      (assert (= (att-value 'cyclic-att ast) 19))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16))
      (assert (= (att-value 'cyclic-att ast) 17))
      (assert (= (att-value 'cyclic-att ast) 18))
      (assert (= (att-value 'cyclic-att ast) 19)))
    ; ------Inverse Access Order------ ;
    (let ((ast (init-basic-cycle-test #f)))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16))
      (assert (= (att-value 'cyclic-att ast) 17))
      (assert (= (att-value 'cyclic-att ast) 18))
      (assert (= (att-value 'cyclic-att ast) 19))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16))
      (assert (= (att-value 'cyclic-att ast) 17))
      (assert (= (att-value 'cyclic-att ast) 18))
      (assert (= (att-value 'cyclic-att ast) 19))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16))
      (assert (= (att-value 'cyclic-att (ast-child 1 ast)) 16)))
    
    (letrec ((ast (init-fibonacci-numbers #t))
             (fibonacci-naive
              (lambda (num)
                (if (< num 2)
                    num
                    (+ (fibonacci-naive (- num 1)) (fibonacci-naive (- num 2))))))
             (fibonacci-accumulated
              (lambda (a1 a2 n)
                (let ((res (+ a1 a2)))
                  (if (> n 2)
                      (fibonacci-accumulated res a1 (- n 1))
                      res)))))
      ;(display (fibonacci-naive 3500))
      (display (att-value 'fibonacci ast 3500))
      ;(display (fibonacci-accumulated 1 0 3500))
      )))