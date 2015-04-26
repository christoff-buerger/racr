; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr core) (atomic-petrinets user-interface))

(define spec                 (create-specification))

(define                      (Boolean) #f)
(define                      (Integer) #f)

(define (->name n)           (ast-child 'name n))
(define (->type n)           (ast-child 'type n))
(define (->source n)         (ast-child 'source n))
(define (->target n)         (ast-child 'target n))
(define (->asignee n)        (ast-child 'asignee n))
(define (->operator n)       (ast-child 'operator n))
(define (->operand1 n)       (ast-child 'operand1 n))
(define (->operand2 n)       (ast-child 'operand2 n))

(define (=edges n)           (ast-children (ast-child 'ActivityEdge* n)))
(define (=variables n)       (ast-children (ast-child 'Variable* n)))
(define (=expressions n)     (ast-children (ast-child 'Expression* n)))
(define (=var n name)        (att-value 'var n name))
(define (=outgoing n)        (att-value 'outgoing n))
(define (=incoming n)        (att-value 'incoming n))
(define (=well-typed? n)     (att-value 'well-typed? n))

(define (&& . a)             (for-all (lambda (x) x) a))
(define (// . a)             (find (lambda (x) x) a))

(with-specification
 spec
 (ast-rule 'Activity->name-Variable*-ActivityNode*-ActivityEdge*)
 (ast-rule 'Variable->name-type-initial)
 (ast-rule 'ActivityEdge->name-source-target)
 (ast-rule 'ControlFlow:ActivityEdge->guard)
 (ast-rule 'ActivityNode->name)
 (ast-rule 'InitialNode:ActivityNode->)
 (ast-rule 'FinalNode:ActivityNode->)
 (ast-rule 'ForkNode:ActivityNode->)
 (ast-rule 'JoinNode:ActivityNode->)
 (ast-rule 'DecisionNode:ActivityNode->)
 (ast-rule 'MergeNode:ActivityNode->)
 (ast-rule 'ExecutableNode:ActivityNode->Expression*)
 (ast-rule 'Expression->assignee-operator)
 (ast-rule 'UnaryExpression:Expression->operand1)
 (ast-rule 'BinaryExpression:Expression->operand1-operand2)
 (compile-ast-specifications 'Activity)
 
 ;;; Name Analysis:
 
 (ag-rule
  outgoing
  (ActivityNode (lambda (n) (filter (lambda (e) (eq? (->source e) (->name n))) (=edges n)))))
 
 (ag-rule
  incoming
  (ActivityNode (lambda (n) (filter (lambda (e) (eq? (->target e) (->name n))) (=edges n)))))
 
 (ag-rule
  var
  (Activity     (lambda (n name) (find (lambda (n) (eq? (->name n) name)) (=variables n)))))
 
 ;;; Type Analysis:
 
 (ag-rule
  well-typed?
  
  (UnaryExpression
   (lambda (n)
     (define ass (=var (->asignee n)))
     (define op (=var (->operand1 n)))
     (and ass op (eq? (->type op) Boolean) (eq? (->type ass) Boolean))))
  
  (BinaryExpression
   (lambda (n)
     (define ass (=var (->asignee n)))
     (define op1 (=var (->operand1 n)))
     (define op2 (=var (->operand2 n)))
     (define (in . l) (memq (->operator n) l))
     (define (op-type t) (and (eq? (->type op1) t) (eq? (->type op2) t)))
     (and ass op1 op2
          (or (and (in + -) (op-type Integer) (eq? (->type ass) Integer))
              (and (in < <= = > >=) (op-type Integer) (eq? (->type ass) Boolean))
              (and (in && //) (op-type Boolean) (eq? (->type ass) Boolean)))))))
 
 ;;; Well-formedness:
 
 (define (in n f s) (f (length (=incoming n)) s))
 (define (out n f s) (f (length (=outgoing n)) s))
 (define (guarded n g)
   (and
    (for-all (lambda (n) (not (ast-subtype? n 'ControlFlow))) (=incoming n))
    (for-all (lambda (n) (if (ast-subtype? n 'ControlFlow) g (not g))) (=outgoing n))))
 
 (ag-rule
  valid?
  (InitialNode    (lambda (n) (and (in n = 0) (out n = 1) (guarded n #f))))
  (FinalNode      (lambda (n) (and (in n = 1) (out n = 0) (guarded n #f))))
  (ForkNode       (lambda (n) (and (in n = 1) (out n > 1) (guarded n #f))))
  (JoinNode       (lambda (n) (and (in n > 1) (out n = 1) (guarded n #f))))
  (DecisionNode   (lambda (n) (and (in n = 1) (out n > 1) (guarded n #t))))
  (MergeNode      (lambda (n) (and (in n > 1) (out n = 1) (guarded n #f))))
  (ExecutableNode (lambda (n) (and (in n = 1) (out n = 1) (guarded n #f)
                                   (for-all =well-typed? (=expressions n))))))
 
 (compile-ag-specifications))