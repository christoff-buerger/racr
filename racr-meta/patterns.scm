; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (racr-meta patterns)
 (export
  specify-pattern
  with-bindings
  create-transformer-for-pattern)
 (import (rnrs) (racr-meta core))
 
 (define pattern-language (create-specification))
 
 ; ******************************************************************************************************** ;
 ;                                                W A R N I N G                                             ;
 ; THE PATTERN LIBRARY IS BROKEN AT THE MOMENT. IT MUST BE PROPERLY REFACTORED FOR THE RACR-META-EDITION.   ;
 ; THE INTERNAL DEPENDENCIES TO FIX ARE:                                                                    ;
 ; ******************************************************************************************************** ;
 (define (ast-rule-subtype? ast-rule1 ast-rule2) #f)
 (define (ast-rule-subtypes ast-rule) #f)
 (define (ast-rule-find-child-context ast-rule context-name) #f)
 (define-syntax with-specification-patterns
   (lambda (x)
     (syntax-case x ()
       ((k spec body ...)
        #`(let* ((spec* spec)
                 (#,(datum->syntax #'k 'specify-pattern)
                  (lambda (att-name distinguished-node fragments references condition)
                    (specify-pattern spec* att-name distinguished-node fragments references condition)))
                 (#,(datum->syntax #'k 'create-transformer-for-pattern)
                  (lambda (node-type pattern-attribute rewrite-function . pattern-arguments)
                    (apply create-transformer-for-pattern spec* node-type pattern-attribute rewrite-function pattern-arguments))))
            (let-syntax ((#,(datum->syntax #'k 'ag-rule)
                          (syntax-rules ()
                            ((_ attribute-name definition (... ...))
                             (ag-rule spec* attribute-name definition (... ...))))))
              body ...))))))
 ; ******************************************************************************************************** ;
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pattern Specification ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (define (specify-pattern spec att-name distinguished-node fragments references condition?)
   (define (process-fragment context type binding children)
     (unless (and
              (or (symbol? context) (integer? context))
              (or (not type) (symbol? type))
              (or (not binding) (symbol? binding)))
       (throw-exception
        "Invalid pattern definition; "
        "Wrong argument type (context, type or binding of fragment)."))
     (create-ast
      pattern-language
      'Node
      (list
       context
       type
       binding
       (create-ast-list
        (map
         (lambda (child)
           (apply process-fragment child))
         children)))))
   (define (process-reference name source target)
     (unless (and (symbol? name) (symbol? source) (symbol? target))
       (throw-exception
        "Invalid pattern definition; "
        "Wrong argument type (name, source and target of references must be symbols)."))
     (create-ast pattern-language 'Ref (list name source target)))
   (define ast
     (create-ast
      pattern-language
      'Pattern
      (list
       (create-ast-list (map (lambda (frag) (apply process-fragment (cons 'racr-nil frag))) fragments))
       (create-ast-list (map (lambda (ref) (apply process-reference ref)) references))
       #f
       spec)))
   ; Resolve symbolic node references (i.e., perform name analysis):
   (rewrite-terminal 'dnode ast (att-value 'lookup-node ast distinguished-node))
   (for-each
    (lambda (ref)
      (let ((source? (att-value 'lookup-node ast (ast-child 'source ref)))
            (target? (att-value 'lookup-node ast (ast-child 'target ref))))
        (if source?
            (rewrite-terminal 'source ref source?)
            (throw-exception
             "Invalid pattern definition; "
             "Undefined reference source " (ast-child 'source ref) "."))
        (if target?
            (rewrite-terminal 'target ref target?)
            (throw-exception
             "Invalid pattern definition; "
             "Undefined reference target " (ast-child 'target ref) "."))))
    (ast-children (ast-child 'Ref* ast)))
   ; Ensure well-formedness of the pattern (valid distinguished node, reachability, typing, unique node naming):
   (unless (att-value 'well-formed? ast)
     (throw-exception
      "Invalid pattern definition; "
      "The pattern is not well-formed."))
   ; Every thing is fine. Thus, add a respective matching attribute to the given specification: 
   (specify-attribute
    spec
    att-name
    (ast-child 'type (ast-child 'dnode ast))
    '*
    #t
    (let ((pmm (att-value 'pmm-code ast))) ; Precompute the PMM => The pattern AST is not in the equation's closure
      (if condition?
          (lambda (n . args)
            (let ((bindings (pmm n)))
              (if (and bindings (apply condition? bindings args))
                  bindings
                  #f)))
          pmm))
    #f))
 
 (define-syntax with-bindings
   (syntax-rules ()
     ((_ ((binding ...) (parameter ...)) body body* ...)
      (lambda (l parameter ...)
        (let ((binding (cdr (assq 'binding l))) ...)
          body
          body* ...)))
     ((_ (binding ...) body body* ...)
      (with-bindings ((binding ...) ()) body body* ...))))
 
 (define create-transformer-for-pattern
   (lambda (spec node-type pattern-attribute rewrite-function . pattern-arguments)
     (let ((ast-rule (specification->find-ast-rule spec node-type)))
       (unless ast-rule
         (throw-exception
          "Cannot construct transformer; "
          "Undefined " node-type " node type."))
       (unless (find
                (lambda (attribute-definition)
                  (eq? (attribute->name attribute-definition) pattern-attribute))
                (symbol->attributes (car (ast-rule->production ast-rule))))
         (throw-exception
          "Cannot construct transformer; "
          "No " pattern-attribute " attribute defined in the context of " node-type " nodes.")))
     (lambda (n)
       (when (and (not (or (ast-bud-node? n) (ast-list-node? n))) (ast-subtype? n node-type))
         (let ((match? (apply att-value pattern-attribute n pattern-arguments)))
           (if match?
               (or
                (apply rewrite-function match? pattern-arguments)
                #t)
               #f))))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pattern Matching Machine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (define pmmi-load-node ; Make already stored node the new current one.
   (lambda (next-instruction index)
     (lambda (current-node node-memory)
       (next-instruction (vector-ref node-memory index) node-memory))))
 
 (define pmmi-store-node ; Store current node for later reference.
   (lambda (next-instruction index)
     (lambda (current-node node-memory)
       (vector-set! node-memory index current-node)
       (next-instruction current-node node-memory))))
 
 (define pmmi-ensure-context-by-name ; Ensure, the current node is certain child & make its parent the new current node.
   (lambda (next-instruction context-name)
     (lambda (current-node node-memory)
       (let ((parent? (ast-has-parent? current-node)))
         (if (and parent? (ast-has-child? context-name parent?) (eq? (ast-child context-name parent?) current-node))
             (next-instruction parent? node-memory)
             #f)))))
 
 (define pmmi-ensure-context-by-index ; Ensure, the current node is certain child & make its parent the new current node.
   (lambda (next-instruction index)
     (lambda (current-node node-memory)
       (let ((parent? (ast-has-parent? current-node)))
         (if (and parent? (>= (ast-num-children parent?) index) (eq? (ast-child index parent?) current-node))
             (next-instruction parent? node-memory)
             #f)))))
 
 (define pmmi-ensure-subtype ; Ensure, the current node is of a certain type or a subtype.
   (lambda (next-instruction super-type)
     (lambda (current-node node-memory)
       (if (and
            (not (ast-list-node? current-node))
            (not (ast-bud-node? current-node))
            (ast-subtype? current-node super-type))
           (next-instruction current-node node-memory)
           #f))))
 
 (define pmmi-ensure-list ; Ensure, the current node is a list node.
   (lambda (next-instruction)
     (lambda (current-node node-memory)
       (if (ast-list-node? current-node)
           (next-instruction current-node node-memory)
           #f))))
 
 (define pmmi-ensure-child-by-name ; Ensure, the current node has a certain child & make the child the new current node.
   (lambda (next-instruction context-name)
     (lambda (current-node node-memory)
       (if (ast-has-child? context-name current-node)
           (next-instruction (ast-child context-name current-node) node-memory)
           #f))))
 
 (define pmmi-ensure-child-by-index ; Ensure, the current node has a certain child & make the child the new current node.
   (lambda (next-instruction index)
     (lambda (current-node node-memory)
       (if (>= (ast-num-children current-node) index)
           (next-instruction (ast-child index current-node) node-memory)
           #f))))
 
 (define pmmi-ensure-node ; Ensure, the current node is a certain, already stored node.
   (lambda (next-instruction index)
     (lambda (current-node node-memory)
       (if (eq? current-node (vector-ref node-memory index))
           (next-instruction current-node node-memory)
           #f))))
 
 (define pmmi-traverse-reference ; Evaluate attribute of current node, ensure value is a node & make it the new current one.
   (lambda (next-instruction reference-name)
     (lambda (current-node node-memory)
       (if (and (not (ast-bud-node? current-node)) (ast-node? (att-value reference-name current-node)))
           (next-instruction (att-value reference-name current-node) node-memory)
           #f))))
 
 (define pmmi-terminate ; Construct association list of all binded nodes.
   (lambda (bindings)
     (let ((bindings ; Precompute list of (key, index) pairs => The pattern AST is not in the instruction's closure
            (map
             (lambda (n)
               (cons (ast-child 'binding n) (att-value 'node-memory-index n)))
             bindings)))
       (lambda (current-node node-memory)
         (map
          (lambda (binding)
            (cons (car binding) (vector-ref node-memory (cdr binding))))
          bindings)))))
 
 (define pmmi-initialize ; First instruction of any PMM program. Allocates memory used to store nodes throughout matching.
   (lambda (next-instruction node-memory-size)
     (lambda (current-node)
       (next-instruction current-node (make-vector node-memory-size)))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pattern Language ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (define load-pattern-language
   (lambda ()
     (with-specification
      pattern-language
      
      (ast-rule 'Pattern->Node*-Ref*-dnode-spec)
      (ast-rule 'Node->context-type-binding-Node*)
      (ast-rule 'Ref->name-source-target)
      (compile-ast-specifications 'Pattern)
      
      ;;; Name Analysis:
      
      (ag-rule ; Given a binding name, find its respective binded node.
       lookup-node
       (Pattern
        (lambda (n name)
          (ast-find-child*
           (lambda (i n)
             (att-value 'local-lookup-node n name))
           (ast-child 'Node* n)))))
      
      (ag-rule
       local-lookup-node
       (Node
        (lambda (n name)
          (if (eq? (ast-child 'binding n) name)
              n
              (ast-find-child*
               (lambda (i n)
                 (att-value 'local-lookup-node n name))
               (ast-child 'Node* n))))))
      
      (ag-rule ; Given a non-terminal, find its respective RACR AST rule.
       lookup-type
       (Pattern
        (lambda (n type)
          (specification->find-ast-rule (ast-child 'spec n) type))))
      
      ;;; Abstract Syntax Tree Query Support:
      
      (ag-rule ; Root of the AST fragment a node is part of.
       fragment-root
       ((Pattern Node*)
        (lambda (n)
          n)))
      
      (ag-rule ; Is the node a fragment root?
       fragment-root?
       ((Pattern Node*)
        (lambda (n) #t))
       ((Node Node*)
        (lambda (n) #f)))
      
      (ag-rule ; List of all references of the pattern.
       references
       (Pattern
        (lambda (n)
          (ast-children (ast-child 'Ref* n)))))
      
      (ag-rule ; List of all named nodes of the pattern.
       bindings
       (Pattern
        (lambda (n)
          (fold-left
           (lambda (result n)
             (append result (att-value 'bindings n)))
           (list)
           (ast-children (ast-child 'Node* n)))))
       (Node
        (lambda (n)
          (fold-left
           (lambda (result n)
             (append result (att-value 'bindings n)))
           (if (ast-child 'binding n) (list n) (list))
           (ast-children (ast-child 'Node* n))))))
      
      (ag-rule ; Number of pattern nodes of the pattern/the subtree spaned by a node (including the node itself).
       nodes-count
       (Pattern
        (lambda (n)
          (fold-left
           (lambda (result n)
             (+ result (att-value 'nodes-count n)))
           0
           (ast-children (ast-child 'Node* n)))))
       (Node
        (lambda (n)
          (fold-left
           (lambda (result n)
             (+ result (att-value 'nodes-count n)))
           1
           (ast-children (ast-child 'Node* n))))))
      
      ;;; Type Analysis:
      
      (ag-rule ; Must the node be a list?
       must-be-list?
       (Node ; A node must be a list if:
        (lambda (n)
          (or
           (eq? (ast-child 'type n) '*) ; (1) the pattern developer defines so,
           (ast-find-child ; (2) any of its children is referenced by index.
            (lambda (i n)
              (integer? (ast-child 'context n)))
            (ast-child 'Node* n))))))
      
      (ag-rule ; Must the node not be a list?
       must-not-be-list?
       (Node ; A node must not be a list if:
        (lambda (n)
          (or
           (and ; (1) the pattern developer defines so,
            (ast-child 'type n)
            (not (eq? (ast-child 'type n) '*)))
           (and ; (2) it is child of a list,
            (not (att-value 'fragment-root? n))
            (att-value 'must-be-list? (ast-parent n)))
           (ast-find-child ; (3) any of its children is referenced by name or must be a list.
            (lambda (i n)
              (or
               (symbol? (ast-child 'context n))
               (att-value 'must-be-list? n)))
            (ast-child 'Node* n))))))
      
      (ag-rule ; List of all types being subject of a Kleene closure, i.e., all list types.
       most-general-list-types
       (Pattern
        (lambda (n)
          (let ((list-types
                 (fold-left
                  (lambda (result ast-rule)
                    (fold-left
                     (lambda (result symbol)
                       (if (and (symbol->kleene? symbol) (not (memq (symbol->non-terminal? symbol) result)))
                           (cons (symbol->non-terminal? symbol) result)
                           result))
                     result
                     (cdr (ast-rule->production ast-rule))))
                  (list)
                  (att-value 'most-concrete-types n))))
            (filter
             (lambda (type1)
               (not
                (find
                 (lambda (type2)
                   (and
                    (not (eq? type1 type2))
                    (ast-rule-subtype? type1 type2)))
                 list-types)))
             list-types)))))
      
      (ag-rule ; List of all types (of a certain type) no other type inherits from.
       most-concrete-types
       (Pattern
        (case-lambda
          ((n)
           (filter
            (lambda (type)
              (null? (ast-rule-subtypes type)))
            (specification->ast-rules (ast-child 'spec n))))
          ((n type)
           (filter
            (lambda (type)
              (null? (ast-rule-subtypes type)))
            (cons type (ast-rule-subtypes type)))))))
      
      (ag-rule ; Satisfies a certain type a node's user defined type constraints?
       valid-user-induced-type?
       (Node
        (lambda (n type kleene?)
          (or
           (not (ast-child 'type n))
           (if (eq? (ast-child 'type n) '*)
               kleene?
               (let ((user-induced-type (att-value 'lookup-type n (ast-child 'type n))))
                 (and
                  user-induced-type
                  (ast-rule-subtype? type user-induced-type))))))))
      
      (ag-rule ; Satisfies a certain type all type constraint of a node and its subtree?
       valid-type?
       (Node
        (lambda (n type kleene?)
          (and
           (not (and (att-value 'must-be-list? n) (not kleene?)))
           (not (and (att-value 'must-not-be-list? n) kleene?))
           (att-value 'valid-user-induced-type? n type kleene?)
           (if kleene?
               (not
                (ast-find-child
                 (lambda (i child)
                   (not
                    (find
                     (lambda (child-type)
                       (att-value 'valid-type? child child-type #f))
                     (att-value 'most-concrete-types n type))))
                 (ast-child 'Node* n)))
               (not
                (ast-find-child
                 (lambda (i child)
                   (let* ((context? (ast-rule-find-child-context type (ast-child 'context child)))
                          (context-types?
                           (cond
                             ((not (and context? (symbol->non-terminal? context?))) (list))
                             ((symbol->kleene? context?) (list (symbol->non-terminal? context?)))
                             (else (att-value 'most-concrete-types n (symbol->non-terminal? context?))))))
                     (not
                      (find
                       (lambda (type)
                         (att-value 'valid-type? child type (symbol->kleene? context?)))
                       context-types?))))
                 (ast-child 'Node* n))))))))
      
      (ag-rule ; Is the pattern satisfiable (a matching AST exists regarding fragment syntax & type constraints)?
       well-typed?
       ((Pattern Node*)
        (lambda (n)
          (or
           (find
            (lambda (type)
              (att-value 'valid-type? n type #f))
            (att-value 'most-concrete-types n))
           (find
            (lambda (type)
              (att-value 'valid-type? n type #t))
            (att-value 'most-general-list-types n))))))
      
      ;;; Reachability:
      
      (ag-rule ; Is the reference connecting two different fragments?
       inter-fragment-reference?
       (Ref
        (lambda (n)
          (not
           (eq?
            (att-value 'fragment-root (ast-child 'source n))
            (att-value 'fragment-root (ast-child 'target n)))))))
      
      (ag-rule ; List of the child contexts to follow to reach the root.
       fragment-root-path
       
       ((Pattern Node*)
        (lambda (n)
          (list)))
       
       ((Node Node*)
        (lambda (n)
          (cons (ast-child 'context n) (att-value 'fragment-root-path (ast-parent n))))))
      
      (ag-rule ; List of the cheapest inter fragment references of a fragment and their respective costs.
       inter-fragment-references
       ((Pattern Node*)
        (lambda (n)
          (define walk-costs ; Sum of distances of a reference's source & target to their roots.
            (lambda (ref)
              (+
               (length (att-value 'fragment-root-path (ast-child 'source ref)))
               (length (att-value 'fragment-root-path (ast-child 'target ref))))))
          (reverse
           (fold-left ; Filter for each target the cheapest inter fragment reference:
            (lambda (result ref)
              (if
               (memp
                (lambda (weighted-ref)
                  (eq?
                   (att-value 'fragment-root (ast-child 'target ref))
                   (att-value 'fragment-root (ast-child 'target (car weighted-ref)))))
                result)
               result
               (cons (cons ref (walk-costs ref)) result)))
            (list)
            (list-sort ; Sort the inter fragment references according to their costs:
             (lambda (ref1 ref2)
               (< (walk-costs ref1) (walk-costs ref2)))
             (filter ; Find all inter fragment references of the fragment:
              (lambda (ref)
                (and
                 (eq? (att-value 'fragment-root (ast-child 'source ref)) n)
                 (att-value 'inter-fragment-reference? ref)))
              (att-value 'references n))))))))
      
      (ag-rule ; List of references best suited to reach other fragments from the distinguished node.
       fragment-walk
       (Pattern
        (lambda (n)
          (let ((dummy-walk
                 (cons
                  (create-ast 'Ref (list #f (ast-child 'dnode n) (ast-child 'dnode n)))
                  0)))
            (let loop ((walked ; List of pairs of already followed references and their total costs.
                        (list dummy-walk))
                       (to-visit ; Fragment roots still to visit.
                        (remq
                         (att-value 'fragment-root (ast-child 'dnode n))
                         (ast-children (ast-child 'Node* n)))))
              (let ((next-walk? ; Find the next inter fragment reference to follow if there is any,...
                     (fold-left ; ...i.e., for every already walked inter fragment reference R,...
                      (lambda (best-next-walk performed-walk)
                        (let ((possible-next-walk ; ...find the best walk reaching a new fragment from its target....
                               (find
                                (lambda (weighted-ref)
                                  (memq
                                   (att-value 'fragment-root (ast-child 'target (car weighted-ref)))
                                   to-visit))
                                (att-value 'inter-fragment-references (ast-child 'target (car performed-walk))))))
                          (cond
                            ((not possible-next-walk) ; ...If no new fragment is reachable from the target of R,...
                             best-next-walk) ; ...keep the currently best walk. Otherwise,...
                            ((not best-next-walk) ; ...if no next best walk has been selected yet,...
                             possible-next-walk) ; ...make the found one the best....
                            (else ; Otherwise,...
                             (let ((costs-possible-next-walk (+ (cdr possible-next-walk) (cdr performed-walk))))
                               (if (< costs-possible-next-walk (cdr best-next-walk)) ; ...select the better one.
                                   (cons (car possible-next-walk) costs-possible-next-walk)
                                   best-next-walk))))))
                      #f
                      walked)))
                (if next-walk? ; If a new fragment can be reached,...
                    (loop ; ...try to find another reachable one. Otherwise,...
                     (append walked (list next-walk?))
                     (remq
                      (att-value 'fragment-root (ast-child 'target (car next-walk?)))
                      to-visit))
                    (map car (cdr walked))))))))) ; ...return the references defining all reachable fragments.
      
      ;;; Well-formedness:
      
      (ag-rule ; Is the pattern specification valid, such that PMM code can be generated?
       well-formed?
       
       (Pattern
        (lambda (n)
          (and
           (att-value 'local-correct? n)
           (not
            (ast-find-child
             (lambda (i n)
               (not (att-value 'well-formed? n)))
             (ast-child 'Node* n))))))
       
       (Node
        (lambda (n)
          (and
           (att-value 'local-correct? n)
           (not
            (ast-find-child
             (lambda (i n)
               (not (att-value 'well-formed? n)))
             (ast-child 'Node* n)))))))
      
      (ag-rule ; Is a certain part of the pattern AST valid?
       local-correct?
       
       (Pattern
        (lambda (n)
          (and
           (ast-node? (ast-child 'dnode n)) ; A distinguished node must be defined, whose...
           (ast-child 'type (ast-child 'dnode n)) ; ...type is user specified and...
           (not (att-value 'must-be-list? (ast-child 'dnode n))) ; ...not a list.
           (= ; All fragments must be reachable from the distinguished node:
            (+ (length (att-value 'fragment-walk n)) 1)
            (ast-num-children (ast-child 'Node* n)))
           (not ; All fragments must be well typed, i.e., there exists an AST where they match:
            (ast-find-child
             (lambda (i n)
               (not (att-value 'well-typed? n)))
             (ast-child 'Node* n))))))
       
       (Node
        (lambda (n)
          (and
           (or ; Binded names must be unique:
            (not (ast-child 'binding n))
            (eq? (att-value 'lookup-node n (ast-child 'binding n)) n))
           (let loop ((children (ast-children (ast-child 'Node* n)))) ; Contexts must be unique:
             (cond
               ((null? children) #t)
               ((find
                 (lambda (child)
                   (eqv? (ast-child 'context (car children)) (ast-child 'context child)))
                 (cdr children))
                #f)
               (else (loop (cdr children)))))))))
      
      ;;; Code generation:
      
      (ag-rule ; Index within node memory. Used during pattern matching to store and later load matched nodes.
       node-memory-index
       
       ((Pattern Node*)
        (lambda (n)
          (if (> (ast-child-index n) 1)
              (+
               (att-value 'node-memory-index (ast-sibling (- (ast-child-index n) 1) n))
               (att-value 'nodes-count (ast-sibling (- (ast-child-index n) 1) n)))
              0)))
       
       ((Node Node*)
        (lambda (n)
          (if (> (ast-child-index n) 1)
              (+
               (att-value 'node-memory-index (ast-sibling (- (ast-child-index n) 1) n))
               (att-value 'nodes-count (ast-sibling (- (ast-child-index n) 1) n)))
              (+ (att-value 'node-memory-index (ast-parent n)) 1)))))
      
      (ag-rule ; Function encoding pattern matching machine (PMM) specialised to match the pattern.
       pmm-code
       (Pattern
        (lambda (n)
          (pmmi-initialize
           (att-value
            'pmm-code:match-fragment
            (ast-child 'dnode n)
            (fold-right
             (lambda (reference result)
               (pmmi-load-node
                (pmmi-traverse-reference
                 (att-value 'pmm-code:match-fragment (ast-child 'target reference) result)
                 (ast-child 'name reference))
                (att-value 'node-memory-index (ast-child 'source reference))))
             (att-value
              'pmm-code:check-references
              n
              (pmmi-terminate (att-value 'bindings n)))
             (att-value 'fragment-walk n)))
           (+ (att-value 'nodes-count n) 1)))))
      
      (ag-rule ; Function encoding PMM specialised to match the fragment the pattern node is part of.
       pmm-code:match-fragment
       (Node
        (lambda (n continuation-code)
          (fold-right
           (lambda (context result)
             (if (integer? context)
                 (pmmi-ensure-context-by-index result context)
                 (pmmi-ensure-context-by-name result context)))
           (att-value 'pmm-code:match-subtree (att-value 'fragment-root n) continuation-code)
           (att-value 'fragment-root-path n)))))
      
      (ag-rule ; Function encoding PMM specialised to match the subtree the pattern node spans.
       pmm-code:match-subtree
       (Node
        (lambda (n continuation-code)
          (let ((store-instruction
                 (pmmi-store-node
                  (fold-right
                   (lambda (child result)
                     (pmmi-load-node
                      (if (integer? (ast-child 'context child))
                          (pmmi-ensure-child-by-index
                           (att-value 'pmm-code:match-subtree child result)
                           (ast-child 'context child))
                          (pmmi-ensure-child-by-name
                           (att-value 'pmm-code:match-subtree child result)
                           (ast-child 'context child)))
                      (att-value 'node-memory-index n)))
                   continuation-code
                   (ast-children (ast-child 'Node* n)))
                  (att-value 'node-memory-index n))))
            (cond
              ((att-value 'must-be-list? n)
               (pmmi-ensure-list store-instruction))
              ((ast-child 'type n)
               (pmmi-ensure-subtype store-instruction (ast-child 'type n)))
              (else store-instruction))))))
      
      (ag-rule ; Function encoding PMM specialised to match the reference integrity of the pattern.
       pmm-code:check-references
       (Pattern
        (lambda (n continuation-code)
          (fold-left
           (lambda (result reference)
             (pmmi-load-node
              (pmmi-traverse-reference
               (pmmi-ensure-node
                result
                (att-value 'node-memory-index (ast-child 'target reference)))
               (ast-child 'name reference))
              (att-value 'node-memory-index (ast-child 'source reference))))
           continuation-code
           (filter
            (lambda (reference)
              (not (memq reference (att-value 'fragment-walk n))))
            (ast-children (ast-child 'Ref* n)))))))
      
      (compile-ag-specifications))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Initialisation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (when (= (specification->phase pattern-language) 1)
   (load-pattern-language)))
