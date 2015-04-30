; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-fuml-activity-diagrams language)
 (export exception: Boolean Integer Undefined && //
         :Activity :Variable :ActivityEdge :ControlFlow :InitialNode :FinalNode :ForkNode
         :JoinNode :DecisionNode :MergeNode :ExecutableNode :UnaryExpression :BinaryExpression
         ->name ->initial =var =valid?)
 (import (rnrs) (racr core) (atomic-petrinets user-interface))
 
 (define spec                 (create-specification))
 
 ; AST Accessors:
 (define (->name n)           (ast-child 'name n))
 (define (->type n)           (ast-child 'type n))
 (define (->initial n)        (ast-child 'initial n))
 (define (->source n)         (ast-child 'source n))
 (define (->target n)         (ast-child 'target n))
 (define (->guard n)          (ast-child 'guard n))
 (define (->assignee n)       (ast-child 'assignee n))
 (define (->operator n)       (ast-child 'operator n))
 (define (->operand1 n)       (ast-child 'operand1 n))
 (define (->operand2 n)       (ast-child 'operand2 n))
 
 ; Attribute Accessors:
 (define (=variables n)       (att-value 'variables n))
 (define (=nodes n)           (att-value 'nodes n))
 (define (=edges n)           (att-value 'edges n))
 (define (=expressions n)     (att-value 'expressions n))
 (define (=var n name)        (att-value 'var n name))
 (define (=node n name)       (att-value 'node n name))
 (define (=outgoing n)        (att-value 'outgoing n))
 (define (=incoming n)        (att-value 'incoming n))
 (define (=well-typed? n)     (att-value 'well-typed? n))
 (define (=valid? n)          (att-value 'valid? n))
 
 ; Type Support:
 (define (Boolean)            #f)
 (define (Integer)            #f)
 (define (Undefined)          #f)
 
 ; Operator support:
 (define (&& . a)             (for-all (lambda (x) x) a))
 (define (// . a)             (find (lambda (x) x) a))
 
 ; AST Constructors:
 (define (:Activity id v n e)
   (create-ast spec 'Activity (list id (create-ast-list v) (create-ast-list n) (create-ast-list e))))
 (define (:Variable id t i)
   (create-ast spec 'Variable (list id t i)))
 (define (:ActivityEdge s t)
   (create-ast spec 'ActivityEdge (list s t)))
 (define (:ControlFlow s t g)
   (create-ast spec 'ControlFlow (list s t g)))
 (define (:InitialNode id)
   (create-ast spec 'InitialNode (list id)))
 (define (:FinalNode id)
   (create-ast spec 'FinalNode (list id)))
 (define (:ForkNode id)
   (create-ast spec 'ForkNode (list id)))
 (define (:JoinNode id)
   (create-ast spec 'JoinNode (list id)))
 (define (:DecisionNode id)
   (create-ast spec 'DecisionNode (list id)))
 (define (:MergeNode id)
   (create-ast spec 'MergeNode (list id)))
 (define (:ExecutableNode id e)
   (create-ast spec 'ExecutableNode (list id (create-ast-list e))))
 (define (:UnaryExpression a op op1)
   (create-ast spec 'UnaryExpression (list a op op1)))
 (define (:BinaryExpression a op op1 op2)
   (create-ast spec 'BinaryExpression (list a op op1 op2)))
 
 ;;; Exceptions:
 
 (define-condition-type fuml-exception &violation make-fuml-exception fuml-exception?)
 (define (exception: message)
   (raise-continuable (condition (make-fuml-exception) (make-message-condition message))))
 
 ;;; AST Scheme:
 
 (with-specification
  spec
  (ast-rule 'Activity->name-Variable*-ActivityNode*-ActivityEdge*)
  (ast-rule 'Variable->name-type-initial)
  (ast-rule 'ActivityEdge->source-target)
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
  (compile-ast-specifications 'Activity))
 
 ;;; Query Support:
 
 (with-specification
  spec
  (ag-rule variables   (Activity       (lambda (n) (ast-children (ast-child 'Variable* n)))))
  (ag-rule nodes       (Activity       (lambda (n) (ast-children (ast-child 'ActivityNode* n)))))
  (ag-rule edges       (Activity       (lambda (n) (ast-children (ast-child 'ActivityEdge* n)))))
  (ag-rule expressions (ExecutableNode (lambda (n) (ast-children (ast-child 'Expression* n))))))
 
 ;;; Name Analysis:
 
 (with-specification
  spec
  
  (define (find-name name l)
    (find (lambda (n) (eq? (->name n) name)) l))
  
  (ag-rule var  (Activity (lambda (n name) (find-name name (=variables n)))))
  (ag-rule node (Activity (lambda (n name) (find-name name (=nodes n)))))
  
  (ag-rule
   outgoing
   (ActivityNode (lambda (n) (filter (lambda (e) (eq? (->source e) (->name n))) (=edges n)))))
  
  (ag-rule
   incoming
   (ActivityNode (lambda (n) (filter (lambda (e) (eq? (->target e) (->name n))) (=edges n))))))
 
 ;;; Type Analysis:
 
 (with-specification
  spec
  
  (ag-rule
   well-typed?
   
   (Variable
    (lambda (n)
      (if (eq? (->type n) Boolean) (boolean? (->initial n)) (integer? (->initial n)))))
   
   (UnaryExpression
    (lambda (n)
      (define ass (=var n (->assignee n)))
      (define op (=var n (->operand1 n)))
      (and ass op (eq? (->type op) Boolean) (eq? (->type ass) Boolean))))
   
   (BinaryExpression
    (lambda (n)
      (define ass (=var n (->assignee n)))
      (define op1 (=var n (->operand1 n)))
      (define op2 (=var n (->operand2 n)))
      (define (in . l) (memq (->operator n) l))
      (define (op-type t) (and (eq? (->type op1) t) (eq? (->type op2) t)))
      (and ass op1 op2
           (or (and (in + -) (op-type Integer) (eq? (->type ass) Integer))
               (and (in < <= = > >=) (op-type Integer) (eq? (->type ass) Boolean))
               (and (in && //) (op-type Boolean) (eq? (->type ass) Boolean))))))))
 
 ;;; Well-formedness:
 
 (with-specification
  spec
  
  (define (in n f s) (f (length (=incoming n)) s))
  (define (out n f s) (f (length (=outgoing n)) s))
  (define (guarded n g)
    (for-all (lambda (n)
               (if (ast-subtype? n 'ControlFlow)
                   (let ((var (=var n (->guard n))))
                     (and g var (eq? (->type var) Boolean)))
                   (not g)))
      (=outgoing n)))
  
  (ag-rule
   valid?
   (Activity       (lambda (n) (for-all =valid? (append (=variables n) (=nodes n) (=edges n)))))
   (Variable       (lambda (n) (=well-typed? n)))
   (ControlFlow    (lambda (n) (let ((v (=var n (->guard n)))) (and v (eq? (->type v) Boolean)))))
   (ActivityEdge   (lambda (n) #t))
   (InitialNode    (lambda (n) (and (in n = 0) (out n = 1) (guarded n #f))))
   (FinalNode      (lambda (n) (and (in n = 1) (out n = 0) (guarded n #f))))
   (ForkNode       (lambda (n) (and (in n = 1) (out n > 1) (guarded n #f))))
   (JoinNode       (lambda (n) (and (in n > 1) (out n = 1) (guarded n #f))))
   (DecisionNode   (lambda (n) (and (in n = 1) (out n > 1) (guarded n #t))))
   (MergeNode      (lambda (n) (and (in n > 1) (out n = 1) (guarded n #f))))
   (ExecutableNode (lambda (n) (and (in n = 1) (out n = 1) (guarded n #f)
                                    (for-all =well-typed? (=expressions n))))))
  
  (compile-ag-specifications)))