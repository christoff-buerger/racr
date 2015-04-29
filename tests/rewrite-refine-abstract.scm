; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr core) (racr testing))

(define run-tests
  (lambda ()
    (let-values (((rt-equation influenced:) (construct-reevaluation-tests)))
      (let* (;;; Test compiler:
             (spec (create-specification))
             (ast
              (with-specification
               spec
               
               (ast-rule 'S->A-D)
               (ast-rule 'A->B<B1)
               (ast-rule 'Aa:A->)
               (ast-rule 'Ab:A->B<B2-t)
               (ast-rule 'Ac:Ab->)
               (ast-rule 'B->)
               (ast-rule 'D->)
               
               (compile-ast-specifications 'S)
               
               (ag-rule
                A-child-count
                (D
                 (rt-equation
                  'A-child-count
                  (lambda (n)
                    (ast-num-children (ast-child 'A (ast-parent n)))))))
               (ag-rule
                A-has-B2-child?
                (D
                 (rt-equation
                  'A-has-B2-child?
                  (lambda (n)
                    (ast-has-child? 'B2 (ast-child 'A (ast-parent n)))))))
               (ag-rule
                A-type
                (D
                 (rt-equation
                  'A-type
                  (lambda (n)
                    (ast-node-type (ast-child 'A (ast-parent n)))))))
               (ag-rule
                A-sub-type
                (D
                 (rt-equation
                  'A-sub-type
                  (lambda (n)
                    (ast-subtype? (ast-child 'A (ast-parent n)) 'A)))))
               (ag-rule
                Ab-sub-type
                (D
                 (rt-equation
                  'Ab-sub-type
                  (lambda (n)
                    (ast-subtype? (ast-child 'A (ast-parent n)) 'Ab)))))
               (ag-rule
                A-super-type
                (D
                 (rt-equation
                  'A-super-type
                  (lambda (n)
                    (ast-subtype? 'A (ast-child 'A (ast-parent n)))))))
               (ag-rule
                Ab-super-type
                (D
                 (rt-equation
                  'Ab-super-type
                  (lambda (n)
                    (ast-subtype? 'Ab (ast-child 'A (ast-parent n)))))))
               
               (ag-rule
                s
                (S
                 (rt-equation
                  's
                  (lambda (n)
                    (att-value 's (ast-child 'A n)))))
                (A
                 (rt-equation
                  's
                  (lambda (n)
                    (att-value 'i (ast-child 'B1 n)))))
                (Ac
                 (rt-equation
                  's
                  (lambda (n)
                    (att-value 'i (ast-child 'B1 n))
                    (att-value 'i (ast-child 'B2 n))))))
               
               (ag-rule
                i
                ((A B1)
                 (rt-equation
                  'i
                  (lambda (n) 1)))
                ((Aa B1)
                 (rt-equation
                  'i
                  (lambda (n) 1)))
                ((Ab B2)
                 (rt-equation
                  'i
                  (lambda (n) 1))))
               
               (ag-rule
                t-value
                (S
                 (rt-equation
                  't-value
                  (lambda (n)
                    (att-value 't-value (ast-child 'A n)))))
                (A
                 (rt-equation
                  't-value
                  (lambda (n)
                    (and (ast-has-child? 't n) (ast-child 't n))))))
               
               (compile-ag-specifications)
               
               (create-ast
                'S
                (list
                 (create-ast
                  'A
                  (list
                   (create-ast 'B (list))))
                 (create-ast 'D (list))))))
             
             ;;; Test compiler AST access functions:
             (S
              (lambda ()
                ast))
             (D
              (lambda ()
                (ast-child 'D ast)))
             (A
              (lambda ()
                (ast-child 'A (S))))
             (B1
              (lambda ()
                (ast-child 'B1 (A))))
             (B2
              (lambda ()
                (ast-child 'B2 (A)))))
        (with-specification
         spec
         
         (influenced:
          (S) 's
          (S) 't-value
          (A) 's
          (A) 't-value
          (D) 'A-child-count
          (D) 'A-has-B2-child?
          (D) 'A-type
          (D) 'A-sub-type
          (D) 'Ab-sub-type
          (D) 'A-super-type
          (D) 'Ab-super-type
          (B1) 'i)
         
         (rewrite-refine (A) 'Aa)
         (influenced:
          (S) 's
          (A) 's
          (D) 'A-type
          (D) 'A-super-type
          (D) 'Ab-super-type
          (B1) 'i)
         
         (rewrite-abstract (A) 'A)
         (influenced:
          (S) 's
          (A) 's
          (D) 'A-type
          (D) 'A-super-type
          (D) 'Ab-super-type
          (B1) 'i)
         
         (rewrite-refine (A) 'Ab (create-ast 'B (list)) #t)
         (influenced:
          (S) 't-value
          (A) 't-value
          (D) 'A-child-count
          (D) 'A-has-B2-child?
          (D) 'A-type
          (D) 'A-super-type
          (D) 'Ab-sub-type
          (B2) 'i)
         
         (rewrite-refine (A) 'Ac)
         (influenced:
          (S) 's
          (A) 's
          (D) 'A-type
          (D) 'Ab-super-type)
         
         (rewrite-abstract (A) 'Ab)
         (influenced:
          (S) 's
          (A) 's
          (D) 'A-type
          (D) 'Ab-super-type)
         
         (rewrite-abstract (A) 'A)
         (influenced:
          (S) 't-value
          (A) 't-value
          (D) 'A-child-count
          (D) 'A-has-B2-child?
          (D) 'A-type
          (D) 'Ab-sub-type
          (D) 'A-super-type)
         
         (rewrite-refine (A) 'Ac (create-ast 'B (list)) #t)
         (influenced:
          (S) 's
          (S) 't-value
          (A) 's
          (A) 't-value
          (D) 'A-child-count
          (D) 'A-has-B2-child?
          (D) 'A-type
          (D) 'Ab-sub-type
          (D) 'A-super-type
          (D) 'Ab-super-type
          (B2) 'i)
         
         (rewrite-abstract (A) 'A)
         (influenced:
          (S) 's
          (S) 't-value
          (A) 's
          (A) 't-value
          (D) 'A-child-count
          (D) 'A-has-B2-child?
          (D) 'A-type
          (D) 'Ab-sub-type
          (D) 'A-super-type
          (D) 'Ab-super-type))))))

(run-tests)