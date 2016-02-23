; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-fuml-activity-diagrams language)
 (export :Activity :Variable :ActivityEdge :ControlFlow :InitialNode :FinalNode :ForkNode
         :JoinNode :DecisionNode :MergeNode :ExecutableNode :UnaryExpression :BinaryExpression
         ->name ->initial ->source ->target
         =variables =edges =v-lookup =e-lookup =initial =valid? =v-accessor =petrinet
         Boolean Integer Undefined && // trace activate-tracing deactivate-tracing exception:)
 (import (rnrs) (racr core) (prefix (atomic-petrinets analyses) pn:))
 
 (define spec                 (create-specification))
 
 ; AST Accessors:
 (define (->Variable* n)      (ast-child 'Variable* n))
 (define (->ActivityNode* n)  (ast-child 'ActivityNode* n))
 (define (->ActivityEdge* n)  (ast-child 'ActivityEdge* n))
 (define (->Expression* n)    (ast-child 'Expression* n))
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
 (define (->* n)              (ast-children n))
 (define (<- n)               (ast-parent n))
 
 ; Attribute Accessors:
 (define (=variables n)       (att-value 'variables n))
 (define (=nodes n)           (att-value 'nodes n))
 (define (=edges n)           (att-value 'edges n))
 (define (=expressions n)     (att-value 'expressions n))
 (define (=v-lookup n name)   (hashtable-ref (att-value 'v-lookup n) name #f))
 (define (=n-lookup n name)   (hashtable-ref (att-value 'n-lookup n) name #f))
 (define (=e-lookup n name)   (hashtable-ref (att-value 'e-lookup n) name #f))
 (define (=source n)          (att-value 'source n))
 (define (=target n)          (att-value 'target n))
 (define (=outgoing n)        (att-value 'outgoing n))
 (define (=incoming n)        (att-value 'incoming n))
 (define (=initial n)         (att-value 'initial n))
 (define (=well-typed? n)     (att-value 'well-typed? n))
 (define (=valid? n)          (att-value 'valid? n))
 (define (=v-token n)         (att-value 'v-token n))
 (define (=v-accessor n)      (att-value 'v-accessor n))
 (define (=petrinet n)        (att-value 'petrinet n))
 (define (=places n)          (att-value 'places n))
 (define (=transitions n)     (att-value 'transitions n))
 (define (=computation n)     (att-value 'computation n))
 
 ; AST Constructors:
 (define (:Activity id v n e)
   (create-ast
    spec 'Activity (list id (create-ast-list v) (create-ast-list n) (create-ast-list e))))
 (define (:Variable id t i)
   (create-ast spec 'Variable (list id t i)))
 (define (:ActivityEdge id s t)
   (create-ast spec 'ActivityEdge (list id s t)))
 (define (:ControlFlow id s t g)
   (create-ast spec 'ControlFlow (list id s t g)))
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
 
 ; Type, Operator & Tracing Support:
 (define-record-type atom     (sealed #t)(opaque #t))
 (define Boolean              (make-atom))
 (define Integer              (make-atom))
 (define Undefined            (make-atom))
 (define print-trace?         #t)
 (define (&& . a)             (for-all (lambda (x) x) a))
 (define (// . a)             (find (lambda (x) x) a))
 (define (trace . message)    (when print-trace? (for-each display message) (newline)))
 (define (activate-tracing)   (set! print-trace? #t))
 (define (deactivate-tracing) (set! print-trace? #f))
 
 ; Exceptions:
 (define-condition-type fuml-exception &violation make-fuml-exception fuml-exception?)
 (define (exception: message)
   (raise-continuable (condition (make-fuml-exception) (make-message-condition message))))
 
 ;;; AST Scheme:
 
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
  
  (compile-ast-specifications 'Activity))
 
 ;;; Query Support:
 
 (with-specification
  spec
  
  (ag-rule
   variables ; List of variables of diagram.
   (Activity       (lambda (n) (->* (->Variable* n)))))
  
  (ag-rule
   nodes ; List of activity nodes of diagram.
   (Activity       (lambda (n) (->* (->ActivityNode* n)))))
  
  (ag-rule
   edges ; List of activity edges of diagram.
   (Activity       (lambda (n) (->* (->ActivityEdge* n)))))
  
  (ag-rule
   expressions ; List of expressions of executable activity node.
   (ExecutableNode (lambda (n) (->* (->Expression* n))))))
 
 ;;; Name Analysis:
 
 (with-specification
  spec
  
  (define (make-symbol-table -> l) ; Hash the entities of a list w.r.t. a certain field.
    (define table (make-eq-hashtable))
    (for-each (lambda (n) (hashtable-set! table (-> n) n)) l)
    table)
  
  (define (make-connection-table -> l) ; Hash all entities of a list (equally keyed in same set).
    (define table (make-eq-hashtable))
    (for-each (lambda (n) (hashtable-update! table (-> n) (lambda (v) (cons n v)) (list))) l)
    table)
  
  (ag-rule
   v-lookup ; Hashmap of all variables of diagram (symbolic name -> variable).
   (Activity       (lambda (n) (make-symbol-table ->name (=variables n)))))
  
  (ag-rule
   n-lookup ; Hashmap of all activity nodes of diagram (symbolic name -> node).
   (Activity       (lambda (n) (make-symbol-table ->name (=nodes n)))))
  
  (ag-rule
   e-lookup ; Hashmap of all activity edges of diagram (symbolic name -> edge).
   (Activity       (lambda (n) (make-symbol-table ->name (=edges n)))))
  
  (ag-rule
   source ; Source-node of activity edge (#f if undefined).
   (ActivityEdge   (lambda (n) (=n-lookup n (->source n)))))
  
  (ag-rule
   target ; Target-node of activity edge (#f if undefined).
   (ActivityEdge   (lambda (n) (=n-lookup n (->target n)))))
  
  (ag-rule
   incoming ; Clustering of nodes w.r.t. target / List of incoming edges of a node.
   (Activity       (lambda (n) (make-connection-table ->target (=edges n))))
   (ActivityNode   (lambda (n) (hashtable-ref (=incoming (<- n)) (->name n)
                                              (list)))))
  
  (ag-rule
   outgoing ; Clustering of nodes w.r.t. source / List of outgoing edges of a node.
   (Activity       (lambda (n) (make-connection-table ->source (=edges n))))
   (ActivityNode   (lambda (n) (hashtable-ref (=outgoing (<- n)) (->name n)
                                              (list)))))
  
  (ag-rule
   initial ; The diagram's initial node.
   (Activity       (lambda (n) (find (lambda (n) (ast-subtype? n 'InitialNode)) (=nodes n))))))
 
 ;;; Type Analysis:
 
 (with-specification
  spec
  
  (ag-rule
   well-typed? ; Are variables initialised and expressions type correct?
   
   (Variable
    (lambda (n)
      (if (eq? (->type n) Boolean) (boolean? (->initial n)) (integer? (->initial n)))))
   
   (UnaryExpression
    (lambda (n)
      (define ass (=v-lookup n (->assignee n)))
      (define op (=v-lookup n (->operand1 n)))
      (and ass op (eq? (->type op) Boolean) (eq? (->type ass) Boolean))))
   
   (BinaryExpression
    (lambda (n)
      (define ass (=v-lookup n (->assignee n)))
      (define op1 (=v-lookup n (->operand1 n)))
      (define op2 (=v-lookup n (->operand2 n)))
      (define (in . l) (memq (->operator n) l))
      (define (op-type t) (and (eq? (->type op1) t) (eq? (->type op2) t)))
      (and ass op1 op2
           (or (and (in + -) (op-type Integer) (eq? (->type ass) Integer))
               (and (in < <= = > >=) (op-type Integer) (eq? (->type ass) Boolean))
               (and (in && //) (op-type Boolean) (eq? (->type ass) Boolean))))))))
 
 ;;; Well-formedness:
 
 (with-specification
  spec
  
  (define (in n f s)          (f (length (=incoming n)) s)) ; Number of incoming edges satisfies f?
  (define (out n f s)         (f (length (=outgoing n)) s)) ; Number of outgoing edges satisfies f?
  (define (guarded n g) ; All outgoing edges of n are guarded/not guarded?
    (define (guarded n)
      (if (ast-subtype? n 'ControlFlow)
          (let ((var (=v-lookup n (->guard n))))
            (and g var (eq? (->type var) Boolean)))
          (not g)))
    (for-all guarded (=outgoing n)))
  
  (ag-rule
   valid? ; Is the diagram well-formed?
   (Variable       (lambda (n) (=well-typed? n)))
   (ActivityEdge   (lambda (n) (eq? (=e-lookup n (->name n)) n) (=source n) (=target n)))
   (ControlFlow    (lambda (n)
                     (and (eq? (=e-lookup n (->name n)) n) (=source n) (=target n)
                          (let ((v (=v-lookup n (->guard n)))) (and v (eq? (->type v) Boolean))))))
   (InitialNode    (lambda (n) (and (in n = 0) (out n = 1) (guarded n #f) (eq? (=initial n) n))))
   (FinalNode      (lambda (n) (and (in n >= 1) (out n = 0) (guarded n #f))))
   (ForkNode       (lambda (n) (and (in n = 1) (out n > 1) (guarded n #f))))
   (JoinNode       (lambda (n) (and (in n > 1) (out n = 1) (guarded n #f))))
   (MergeNode      (lambda (n) (and (in n >= 1) (out n = 1) (guarded n #f))))
   (DecisionNode   (lambda (n) (and (in n = 1) (out n >= 1) (guarded n #t))))
   (ExecutableNode (lambda (n) (and (in n = 1) (out n = 1) (guarded n #f)
                                    (for-all =well-typed? (=expressions n)))))
   (Activity
    (lambda (n)
      (and (=initial n)
           (for-all =valid? (=variables n))
           (for-all =valid? (=nodes n))
           (for-all =valid? (=edges n)))))))
 
 ;;; Code Generation:
 
 ; The execution semantics of activity diagrams are given by their translation to Petri nets:
 ;  (1) For each variable exists a place with a single token whose value is the variable's value.
 ;  (2) For each node an equally named place exists.
 ;  (3) The activity edges connecting a node N with its successor nodes S are realised as a single
 ;      or several transitions which consume a token of N's place and push it to a place of a
 ;      certain or all S. The rules are:
 ;      (a) For each successor of a decision one transition is constructed. It is enabled iff the
 ;          token value of its constrainted variable is true. Its source is the decision's place
 ;          and its target the respective successor's place.
 ;      (b) For each predecessor of a merge one transition is constructed. Its source is the
 ;          respective predecessor's place and its target the merge's place.
 ;      (c) For all successors of a fork a single connecting transition is constructed. Its single
 ;          source is the fork's place and its targets are the successors' places.
 ;      (d) For all predecessors of a join a single connecting transition is constructed. Its
 ;          sources are the predecessors' places and its single target is the join's place.
 ;      (e) For each further edge between nodes another transition between their respective places
 ;          is constructed.
 ;      (f) When a transition is fired it consumes a single token from each source and places a
 ;          single token in each target.
 ;  (4) The execution semantics of executable nodes are:
 ;      (a) The expressions of executable nodes are executed IMMEDIATELY BEFORE a token is placed
 ;          in their respective place.
 ;      (b) When executed, the expressions of executable nodes read and write the token values of
 ;          the respective places of their variables.
 ; From (1-3) the following construction is deduced:
 ;  (A) Places are constructed by attributes associated with nodes and variables:
 ;      (a) Each variable constructs one place.
 ;      (b) Each node constructs one place.
 ;  (B) Transitions are constructed by attributes associated with nodes:
 ;      (a) Nodes construct their predecessor transitions, except for fork predecessors.
 ;      (b) Nodes, except forks, do not construct their successor transitions.
 
 (with-specification
  spec
  
  (define (>>? n) ; Construct incoming Petri net arc for activity edge.
    (if (ast-subtype? n 'ControlFlow)
        (pn::Arc (->source n) (list (=v-accessor (=v-lookup n (->guard n)))))
        (pn::Arc (->source n) (list (lambda (t) #t)))))
  (define (n>> n) ; Construct outgoing Petri net arc for activity edge.
    (pn::Arc (->target n) (=computation (=target n))))
  
  (ag-rule
   v-token ; The Petri net token encoding the runtime value of the variable.
   (Variable       (lambda (n) (ast-child 1 (pn:->Token* (=places n))))))
  
  (ag-rule
   v-accessor ; Function returning the runtime value of the variable.
   (Variable       (lambda (n) (let ((token (=v-token n)))
                                 (lambda x (pn:->value token))))))
  
  (ag-rule
   petrinet ; The Petri net simulating the activity diagram's execution.
   (Activity       (lambda (n) (pn::AtomicPetrinet (=places n) (=transitions n)))))
  
  (ag-rule
   places ; List of places of the simulating Petri net.
   (Activity       (lambda (n) (append (map =places (=variables n)) (map =places (=nodes n)))))
   (ActivityNode   (lambda (n) (pn::Place (->name n))))
   (InitialNode    (lambda (n) (pn::Place (->name n) (pn::Token #t))))
   (Variable       (lambda (n)
                     (pn::Place
                      (string->symbol (string-append "@" (symbol->string (->name n))))
                      (pn::Token (->initial n))))))
  
  (ag-rule
   transitions ; List of transitions of the simulating Petri net.
   
   (Activity
    (lambda (n)
      (fold-left (lambda (result n) (append (=transitions n) result)) (list) (=nodes n))))
   
   (ActivityNode
    (lambda (n)
      (fold-left
       (lambda (transitions incoming)
         (if (ast-subtype? (=source incoming) 'ForkNode)
             transitions
             (cons
              (pn::Transition
               (->name incoming)
               (list (>>? incoming))
               (list (n>> incoming)))
              transitions)))
       (list)
       (=incoming n))))
   
   (JoinNode
    (lambda (n)
      (define incoming (=incoming n))
      (list
       (pn::Transition
        (->name (car incoming))
        (map >>? incoming)
        (list (n>> (car incoming)))))))
   
   (ForkNode
    (lambda (n)
      (define incoming (car (=incoming n)))
      (define outgoing (=outgoing n))
      (list
       (pn::Transition
        (->name incoming)
        (list (>>? incoming))
        (list (n>> incoming)))
       (pn::Transition
        (->name (car outgoing))
        (list (>>? (car outgoing)))
        (map n>> outgoing))))))
  
  (ag-rule
   computation ; Function encoding the execution semantics of expressions and nodes.
   
   (ActivityNode
    (lambda (n)
      (define executed (->name n))
      (lambda x
        (trace executed)
        (list #t))))
   
   (ExecutableNode
    (lambda (n)
      (define executed (->name n))
      (define computations (map =computation (=expressions n)))
      (lambda x
        (trace executed)
        (for-each (lambda (f) (f)) computations)
        (list #t))))
   
   (UnaryExpression
    (lambda (n)
      (define assignee (=v-token (=v-lookup n (->assignee n))))
      (define op1 (=v-accessor (=v-lookup n (->operand1 n))))
      (define op (->operator n))
      (lambda () (rewrite-terminal 'value assignee (op (op1))))))
   
   (BinaryExpression
    (lambda (n)
      (define assignee (=v-token (=v-lookup n (->assignee n))))
      (define op1 (=v-accessor (=v-lookup n (->operand1 n))))
      (define op2 (=v-accessor (=v-lookup n (->operand2 n))))
      (define op (->operator n))
      (lambda () (rewrite-terminal 'value assignee (op (op1) (op2))))))))
 
 (compile-ag-specifications spec))