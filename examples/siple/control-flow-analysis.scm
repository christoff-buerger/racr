; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple control-flow-analysis)
 (export
  specify-control-flow-analysis)
 (import (rnrs) (racr core))
 
 (define union
   (lambda (s1 s2)
     (cond
       ((null? s1) s2)
       ((null? s2) s1)
       ((memq (car s1) s2) (union (cdr s1) s2))
       (else (cons (car s1) (union (cdr s1) s2))))))
 
 (define specify-control-flow-analysis
   (lambda (siple-specification)
     (with-specification
      siple-specification
      
      (ag-rule
       ; For every statement, collect all statements within its subtree that can be executed before
       ; terminating the procedure the statement is part of:
       cf-local-exits
       
       (Statement
        (lambda (n)
          (apply
           append
           (if (att-value 'cf-local-exit? n)
               (list n)
               (list))
           (map
            (lambda (n)
              (if (ast-list-node? n)
                  (apply
                   append
                   (map
                    (lambda (n)
                      (att-value 'cf-local-exits n))
                    (ast-children n)))
                  (att-value 'cf-local-exits n)))
            (filter
             ast-node?
             (ast-children n))))))
       
       (ProcedureDeclaration ; Nested procedure declarations define their own local control-flow, i.e.,...
        (lambda (n)
          (list)))) ; ...they do not contribute to the exit points of their enclosing procedure.
      
      (ag-rule
       ; For every statement, derive if it can be the last statement executed before terminating the procedure it is part of:
       cf-local-exit?
       
       ; We indicate, that a statement can be the last executed before terminate a procedure application by adding
       ; the procedure itself to the statement's continuations:
       (Statement
        (lambda (n)
          (memq (att-value 'procedure-in-context n) (att-value 'cf-continuations n)))))
      
      (ag-rule
       ; For every statement S, derive the set of statements that can be executed after the execution of S:
       cf-continuations
       
       (Statement
        (lambda (n)
          (att-value 'cf-dangling-continuations n)))
       
       (Block
        (lambda (n)
          (if (= (ast-num-children (ast-child 'Statement* n)) 0)
              (att-value 'cf-dangling-continuations n)
              (list (ast-child 1 (ast-child 'Statement* n))))))
       
       (If
        (lambda (n)
          (list (ast-child 'Condition n))))
       
       (While
        (lambda (n)
          (list (ast-child 'Condition n))))
       
       (VariableAssignment
        (lambda (n)
          (list (ast-child 'RHand n))))
       
       (ProcedureReturn
        (lambda (n)
          (if (> (ast-num-children (ast-child 'Expression* n)) 0)
              (list (ast-child 1 (ast-child 'Expression* n)))
              (list (att-value 'procedure-in-context n)))))
       
       (Write
        (lambda (n)
          (list (ast-child 'Expression n))))
       
       (Read
        (lambda (n)
          (list (ast-child 'Expression n))))
       
       (ProcedureCall
        (lambda (n)
          (if (> (ast-num-children (ast-child 'Arguments n)) 0)
              (list (ast-child 1 (ast-child 'Arguments n)))
              (att-value 'cf-dangling-continuations n))))
       
       (UnaryExpression
        (lambda (n)
          (list (ast-child 'Operand n))))
       
       (BinaryExpression
        (lambda (n)
          (list (ast-child 'Operand1 n)))))
      
      (ag-rule
       ; For every statement S, derive the statements whose execution can still be dangling because of statements that have been executed before S:
       cf-dangling-continuations
       
       ((Block Statement*)
        (lambda (n)
          (let ((i (ast-child-index n)))
            (if (= i (ast-num-children (ast-parent n)))
                (att-value 'cf-dangling-continuations (ast-parent n))
                (list (ast-sibling (+ i 1) n))))))
       
       ((If Condition)
        (lambda (n)
          (union
           (list (ast-sibling 'Body n))
           (if (> (ast-num-children (ast-sibling 'Alternative n)) 0)
               (list (ast-child 1 (ast-sibling 'Alternative n)))
               (att-value 'cf-dangling-continuations (ast-parent n))))))
       
       ((While Condition)
        (lambda (n)
          (union
           (list (ast-sibling 'Body n))
           (att-value 'cf-dangling-continuations (ast-parent n)))))
       
       ((While Body)
        (lambda (n)
          (union
           (list (ast-parent n))
           (att-value 'cf-dangling-continuations (ast-parent n)))))
       
       ((VariableAssignment RHand)
        (lambda (n)
          (list (ast-sibling 'LHand n))))
       
       ((ProcedureReturn Expression*)
        (lambda (n)
          (list (att-value 'procedure-in-context n))))
       
       ((ProcedureDeclaration Body)
        (lambda (n)
          (list (ast-parent n)))) ; A procedure declaration is by default a dangling continuation of its body (see "cf-local-exit?").
       
       ((ProcedureCall Arguments)
        (lambda (n)
          (let ((i (ast-child-index n)))
            (if (= i (ast-num-children (ast-parent n)))
                (att-value 'cf-dangling-continuations (ast-parent n))
                (list (ast-sibling (+ i 1) n))))))
       
       ((BinaryExpression Operand1)
        (lambda (n)
          (list (ast-sibling 'Operand2 n)))))))))