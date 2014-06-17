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
               
               (ast-rule 'S->A)
               (ast-rule 'A->B-C)
               (ast-rule 'B->)
               (ast-rule 'Ba:B->terminal)
               (ast-rule 'C->D)
               (ast-rule 'D->terminal)
               
               (compile-ast-specifications 'S)
               
               (ag-rule
                att
                (S
                 (rt-equation
                  'att
                  (lambda (n)
                    (att-value 'att (ast-child 1 n)))))
                (A
                 (rt-equation
                  'att
                  (lambda (n)
                    (att-value 'att (ast-child 1 n)))))
                ((A B)
                 (rt-equation
                  'att
                  (lambda (n)
                    (att-value 'att (ast-sibling 2 n)))))
                (Ba
                 (rt-equation
                  'att
                  (lambda (n)
                    (ast-child 1 n))))
                ((A C)
                 (rt-equation
                  'att
                  (lambda (n)
                    (att-value 'att (ast-child 1 n)))))
                (D
                 (rt-equation
                  'att
                  (lambda (n)
                    (ast-child 1 n)))))
               
               (ag-rule
                att2
                ((A C)
                 (rt-equation
                  'att2
                  (lambda (n)
                    (att-value 'att (ast-sibling 1 n)))))
                ((C D)
                 (rt-equation
                  'att2
                  (lambda (n)
                    (att-value 'att2 (ast-parent n))))))
               
               (compile-ag-specifications)
               
               ; Attribute dependencies (* = att; # = att2):
               ;      S *
               ;        |
               ;      A *
               ;       /
               ;      //----\
               ;    B *--* C #
               ;         | | |
               ;         * D #
               (create-ast
                'S
                (list
                 (create-ast
                  'A
                  (list
                   (create-ast 'B (list))
                   (create-ast
                    'C
                    (list
                     (create-ast 'D (list 1))))))))))
             
             ;;; Test compiler AST access functions:
             (S
              (lambda ()
                ast))
             (A
              (lambda()
                (ast-child 1 (S))))
             (B (lambda()
                  (ast-child 1 (A))))
             (C
              (lambda()
                (ast-child 2 (A))))
             (D
              (lambda()
                (ast-child 1 (C)))))
        (with-specification
         spec
         
         (influenced:
          (S) 'att
          (A) 'att
          (B) 'att
          (C) 'att
          (D) 'att
          (C) 'att2
          (D) 'att2)
         
         (rewrite-terminal 1 (D) 10)
         (influenced:
          (S) 'att
          (A) 'att
          (B) 'att
          (C) 'att
          (D) 'att
          (C) 'att2
          (D) 'att2)
         
         (rewrite-subtree (B) (create-ast 'Ba (list -10)))
         (influenced:
          (S) 'att
          (A) 'att
          (B) 'att
          (C) 'att2
          (D) 'att2)
         
         (rewrite-terminal 1 (D) 20)
         (influenced:
          (C) 'att
          (D) 'att)
         
         (rewrite-subtree (B) (create-ast 'B (list)))
         (influenced:
          (S) 'att
          (A) 'att
          (B) 'att
          (C) 'att2
          (D) 'att2)
         
         (rewrite-terminal 1 (D) 1)
         (influenced:
          (S) 'att
          (A) 'att
          (B) 'att
          (C) 'att
          (D) 'att
          (C) 'att2
          (D) 'att2))))))

(run-tests)