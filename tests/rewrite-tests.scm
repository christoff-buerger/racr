; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

; The following rewrite tests verify, that only attributes influenced by rewrites are flushed. For that purpose we maintain for
; every attribute instance an evaluation-counter which states how often the instance has been evaluated. The value of instances
; is the value of their respective evaluation counter. To simulate dependencies to other nodes and attribute instances, attribute
; equations contain respective AST accesses. Given such a setting, incremental attribute evaluation can be tested by
; simply comparing the value of attribute instances to their expected number of evaluations.

#!r6rs

(import (rnrs) (racr))

(define run-tests
  (lambda ()
    (let* (;;; Test compiler:
           (state-table (make-hashtable equal-hash equal? 50))
           (next-state
            (lambda (att n)
              (let ((result (+ (hashtable-ref state-table (cons att n) 0) 1)))
                (hashtable-set! state-table (cons att n) result)
                result)))
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
               (lambda (n)
                 (att-value 'att (ast-child 1 n))
                 (next-state 'att 'S)))
              (A
               (lambda (n)
                 (att-value 'att (ast-child 1 n))
                 (next-state 'att 'A)))
              ((A B)
               (lambda (n)
                 (att-value 'att (ast-sibling 2 n))
                 (next-state 'att 'B)))
              (Ba
               (lambda (n)
                 (ast-child 1 n)
                 (next-state 'att 'B)))
              ((A C)
               (lambda (n)
                 (att-value 'att (ast-child 1 n))
                 (next-state 'att 'C)))
              (D
               (lambda (n)
                 (ast-child 1 n)
                 (next-state 'att 'D))))
             
             (ag-rule
              att2
              ((A C)
               (lambda (n)
                 (att-value 'att (ast-sibling 1 n))
                 (next-state 'att2 'C)))
              ((C D)
               (lambda (n)
                 (att-value 'att2 (ast-parent n))
                 (next-state 'att2 'D))))
             
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
            (S ast)
            (A
             (lambda()
               (ast-child 1 S)))
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
       
       (assert (= (att-value 'att S) 1))
       (assert (= (att-value 'att (A)) 1))
       (assert (= (att-value 'att (B)) 1))
       (assert (= (att-value 'att (C)) 1))
       (assert (= (att-value 'att (D)) 1))
       (assert (= (att-value 'att2 (C)) 1))
       (assert (= (att-value 'att2 (D)) 1))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 'att S) 1))
       (assert (= (att-value 'att (A)) 1))
       (assert (= (att-value 'att (B)) 1))
       (assert (= (att-value 'att (C)) 1))
       (assert (= (att-value 'att (D)) 1))
       (assert (= (att-value 'att2 (C)) 1))
       (assert (= (att-value 'att2 (D)) 1))
       
       (rewrite-terminal 1 (D) 10) ; Influences: D.att, C.att, B.att, A.att, S.att, C.att2, D.att2
       (assert (= (att-value 'att S) 2))
       (assert (= (att-value 'att (A)) 2))
       (assert (= (att-value 'att (B)) 2))
       (assert (= (att-value 'att (C)) 2))
       (assert (= (att-value 'att (D)) 2))
       (assert (= (att-value 'att2 (C)) 2))
       (assert (= (att-value 'att2 (D)) 2))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 'att S) 2))
       (assert (= (att-value 'att (A)) 2))
       (assert (= (att-value 'att (B)) 2))
       (assert (= (att-value 'att (C)) 2))
       (assert (= (att-value 'att (D)) 2))
       (assert (= (att-value 'att2 (C)) 2))
       (assert (= (att-value 'att2 (D)) 2))
       
       (rewrite-terminal 1 (D) (ast-child 1 (D))) ; No change at all => no attribute caches are flushed
       (assert (= (att-value 'att S) 2))
       (assert (= (att-value 'att (A)) 2))
       (assert (= (att-value 'att (B)) 2))
       (assert (= (att-value 'att (C)) 2))
       (assert (= (att-value 'att (D)) 2))
       (assert (= (att-value 'att2 (C)) 2))
       (assert (= (att-value 'att2 (D)) 2))
       
       (rewrite-node (B) (create-ast 'Ba (list -10))) ; Influences: B.att, A.att, S.att, C.att2, D.att2
       (assert (= (att-value 'att S) 3))
       (assert (= (att-value 'att (A)) 3))
       (assert (= (att-value 'att (B)) 3))
       (assert (= (att-value 'att (C)) 2))
       (assert (= (att-value 'att (D)) 2))
       (assert (= (att-value 'att2 (C)) 3))
       (assert (= (att-value 'att2 (D)) 3))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 'att S) 3))
       (assert (= (att-value 'att (A)) 3))
       (assert (= (att-value 'att (B)) 3))
       (assert (= (att-value 'att (C)) 2))
       (assert (= (att-value 'att (D)) 2))
       (assert (= (att-value 'att2 (C)) 3))
       (assert (= (att-value 'att2 (D)) 3))
       
       (rewrite-terminal 1 (D) 20) ; Influences: D.att, C.att
       (assert (= (att-value 'att S) 3))
       (assert (= (att-value 'att (A)) 3))
       (assert (= (att-value 'att (B)) 3))
       (assert (= (att-value 'att (C)) 3))
       (assert (= (att-value 'att (D)) 3))
       (assert (= (att-value 'att2 (C)) 3))
       (assert (= (att-value 'att2 (D)) 3))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 'att S) 3))
       (assert (= (att-value 'att (A)) 3))
       (assert (= (att-value 'att (B)) 3))
       (assert (= (att-value 'att (C)) 3))
       (assert (= (att-value 'att (D)) 3))
       (assert (= (att-value 'att2 (C)) 3))
       (assert (= (att-value 'att2 (D)) 3))
       
       (rewrite-node (B) (create-ast 'B (list))) ; Influences: B.att, A.att, S.att, C.att2, D.att2
       (assert (= (att-value 'att S) 4))
       (assert (= (att-value 'att (A)) 4))
       (assert (= (att-value 'att (B)) 4))
       (assert (= (att-value 'att (C)) 3))
       (assert (= (att-value 'att (D)) 3))
       (assert (= (att-value 'att2 (C)) 4))
       (assert (= (att-value 'att2 (D)) 4))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 'att S) 4))
       (assert (= (att-value 'att (A)) 4))
       (assert (= (att-value 'att (B)) 4))
       (assert (= (att-value 'att (C)) 3))
       (assert (= (att-value 'att (D)) 3))
       (assert (= (att-value 'att2 (C)) 4))
       (assert (= (att-value 'att2 (D)) 4))
       
       (rewrite-terminal 1 (D) 1) ; Influences: D.att, C.att, B.att, A.att, S.att, C.att2, D.att2
       (assert (= (att-value 'att S) 5))
       (assert (= (att-value 'att (A)) 5))
       (assert (= (att-value 'att (B)) 5))
       (assert (= (att-value 'att (C)) 4))
       (assert (= (att-value 'att (D)) 4))
       (assert (= (att-value 'att2 (C)) 5))
       (assert (= (att-value 'att2 (D)) 5))
       ; -----Access Again (by Cache)----- ;
       (assert (= (att-value 'att S) 5))
       (assert (= (att-value 'att (A)) 5))
       (assert (= (att-value 'att (B)) 5))
       (assert (= (att-value 'att (C)) 4))
       (assert (= (att-value 'att (D)) 4))
       (assert (= (att-value 'att2 (C)) 5))
       (assert (= (att-value 'att2 (D)) 5))))))