; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (racr)
 (export
  ; RACR specification interface:
  (rename (make-racr-specification create-specification))
  (rename (racr-specification-specification-phase specification-phase))
  with-specification
  (rename (specify-ast-rule ast-rule))
  (rename (specify-ag-rule ag-rule))
  specify-attribute
  compile-ast-specifications
  compile-ag-specifications
  ; AST annotation interface:
  ast-weave-annotations
  ast-annotation?
  ast-annotation
  ast-annotation-set!
  ast-annotation-remove!
  ; AST & attribute access interface:
  create-ast
  create-ast-list
  ast-node-type
  ast-list-node?
  ast-subtype?
  ast-parent
  ast-child
  ast-sibling
  ast-child-index
  ast-num-children
  ast-children
  ast-for-each-child
  ast-find-child
  att-value
  ; Rewrite interface:
  rewrite-node
  rewrite-terminal
  rewrite-insert
  rewrite-add
  rewrite-delete
  ; Utility interface:
  print-ast
  racr-exception?)
 (import (rnrs) (rnrs mutable-pairs))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                             Internal Data Structures                                                           ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (define-record-type racr-nil-record (sealed #t) (opaque #t)) ; Constructor for unique entities internally used by the RACR system
 (define racr-nil (make-racr-nil-record)) ; Unique value indicating undefined RACR entities
 
 ; Record type representing RACR compiler specifications. A compiler specification consists of arbitrary many AST rule, attribute and
 ; rewrite specifications, all aggregated into a set of rules stored in a non-terminal-symbol -> ast-rule hashtable, an actual
 ; compiler specification phase and a distinguished start symbol. The specification phase is an internal flag indicating the RACR system
 ; the compiler's specification progress. Possible phases are:
 ; 1 : AST specification
 ; 2 : AG specification
 ; 3 : Rewrite specification
 ; 4 : Specification finished
 (define-record-type racr-specification
   (fields (mutable specification-phase) rules (mutable start-symbol))
   (protocol
    (lambda (new)
      (lambda ()
        (new 1 (make-eq-hashtable 50) racr-nil)))))
 
 ; Record type for AST specifcation rules; An AST rule has a reference to the RACR specification it belongs to and consist of
 ; its symbolic encoding, a production (i.e., a list of its symbols), an optional super-type, a list of its sub-types and a list
 ; of its attribute definitions, whereupon symbols are #(name-as-scheme-symbol non-terminal? klenee? context-name-as-scheme-symbol)
 ; quadrupel.
 (define-record-type ast-rule
   (fields specification as-symbol (mutable production) (mutable supertype) (mutable subtypes) (mutable attributes)))
 
 ; Record type for attribute definitions. An attribute definition has a certain name, a definition context consisting
 ; of an AST rule and an attribute position (i.e., a (ast-rule position) pair), an equation, and an optional
 ; circularity-definition needed for circular attributes' fix-point computations. Further, attribute definitions
 ; specify whether the value of instances of the defined attribute are cached. Circularity-definitions are
 ; (bottom-value equivalence-function) pairs, whereby bottom-value is the value fix-point computations start with and
 ; equivalence-functions are used to decide whether a fix-point is reached or not (i.e., equivalence-functions are arbitrary
 ; functions of arity two computing whether two given arguments are equal or not).
 (define-record-type attribute-definition
   (fields name context equation circularity-definition cached?))
 
 ; INTERNAL FUNCTION: Given an attribute instance, check if it is permitted to depend on itself (i.e., be circular) or not.
 ; Return #t, if it can be circular, #f otherwise.
 (define attribute-definition-circular?
   (lambda (att)
     (attribute-definition-circularity-definition att)))
 
 ; INTERNAL FUNCTION: Given an attribute definition, return whether it specifies a synthesized attribute or not
 (define attribute-definition-synthesized?
   (lambda (att-def)
     (= (cdr (attribute-definition-context att-def)) 0)))
 
 ; INTERNAL FUNCTION: Given an attribute definition, return whether it specifies an inherited attribute or not
 (define attribute-definition-inherited?
   (lambda (att-def)
     (not (attribute-definition-synthesized? att-def))))
 
 ; Record type for AST nodes. AST nodes have a reference to the evaluator state used for evaluating their attributes and
 ; rewrites, the AST rule they represent a context of, their parent, children, attribute instances, attributes they influence
 ; and annotations.
 (define-record-type node
   (fields
    (mutable evaluator-state)
    ast-rule
    (mutable parent)
    (mutable children)
    (mutable attributes)
    (mutable attribute-influences)
    (mutable annotations))
   (protocol
    (lambda (new)
      (lambda (ast-rule parent children)
        (new
         #f
         ast-rule
         parent
         children
         (list)
         (list)
         (list))))))
 
 ; INTERNAL FUNCTION: Given a node, return whether it is a terminal or not.
 (define node-terminal?
   (lambda (n)
     (eq? (node-ast-rule n) 'terminal)))
 
 ; INTERNAL FUNCTION: Given a node, return whether it is a non-terminal or not.
 (define node-non-terminal?
   (lambda (n)
     (not (node-terminal? n))))
 
 ; Record type for attribute instances of a certain attribute definition, associated with a certain node (context),
 ; dependencies, influences, a value cache, a cycle cache and an optional cache for the last arguments with
 ; which the attribute has been evaluated. 
 (define-record-type attribute-instance
   (fields
    definition
    (mutable context)
    (mutable node-dependencies)
    (mutable attribute-dependencies)
    (mutable attribute-influences)
    value-cache
    cycle-cache
    (mutable args-cache))
   (protocol
    (lambda (new)
      (lambda (definition context)
        (new
         definition
         context
         (list)
         (list)
         (list)
         (make-hashtable equal-hash equal? 1)
         (make-hashtable equal-hash equal? 1)
         racr-nil)))))
 
 ; Record type representing the internal state of RACR systems throughout their execution, i.e., while evaluating attributes and rewriting
 ; ASTs. An evaluator state consists of a flag indicating if the AG currently performs a fix-point evaluation, a flag indicating if throughout
 ; a fix-point iteration the value of an attribute changed and an attribute evaluation stack used for dependency tracking.
 (define-record-type evaluator-state
   (fields (mutable ag-in-cycle?) (mutable ag-cycle-change?) (mutable att-eval-stack))
   (protocol
    (lambda (new)
      (lambda ()
        (new #f #f (list))))))
 
 ; INTERNAL FUNCTION: Given an evaluator state, return whether it represents an evaluation in progress or not; If it represents an
 ; evaluation in progress return the current attribute in evaluation, otherwise #f.
 (define evaluator-state-in-evaluation?
   (lambda (state)
     (and (not (null? (evaluator-state-att-eval-stack state))) (car (evaluator-state-att-eval-stack state)))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                      Utility                                                                   ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (define object->string
   (lambda (x)
     (call-with-string-output-port
      (lambda (port)
        (display x port)))))
 
 (define-condition-type racr-exception &non-continuable make-racr-exception racr-exception?)
 
 (define-syntax throw-exception
   (syntax-rules ()
     ((_ m-part ...)
      (raise
       (condition
        (make-racr-exception)
        (make-message-condition
         (string-append
          "RACR exception: "
          (let ((m-part* m-part))
            (if (string? m-part*)
                m-part*
                (string-append "[" (object->string m-part*) "]"))) ...)))))))
 
 (define-syntax check-arguments
  (syntax-rules ()
    ((_ args ...)
     (begin
       (if (not args)
         (throw-exception "Unexpected argument; Check: " 'args ".")) ...))))
 
 (define-syntax define-api-function
   (syntax-rules ()
     ((_ name ((parameter parameter-check) ...) body ...)
      (define name
        (lambda (parameter ...)
          (if (not parameter-check)
              (throw-exception "Unexpected " parameter " argument; Check: " 'parameter-check ".")) ...
          body ...)))))
 
 ; INTERNAL FUNCTION: Procedure sequentially applying a function on all the AST rules of a set of rules which inherit,
 ; whereby super-types are processed before their sub-types.
 (define apply-wrt-ast-inheritance
   (lambda (func rules)
     (let* ((resolved ; The set of all AST rules that are already processed....
             (filter ; ...Initially it consists of all the rules that have no super-types.
              (lambda (rule*)
                (not (ast-rule-supertype rule*)))
              rules))
            (to-check ; The set of all AST rules that still must be processed....
             (filter ; ...Initially it consists of all the rules that have super-types.
              (lambda (rule*)
                (ast-rule-supertype rule*))
              rules)))
       (let loop ()
         (let ((to-resolve ; ...Find a rule that still must be processed and...
                (find
                 (lambda (rule*)
                   (memq (ast-rule-supertype rule*) resolved)) ; ...whose super-type already has been processed....
                 to-check)))
           (if to-resolve ; ...Iff such a rule exists,...
               (begin
                 (func to-resolve) ; ...process it,...
                 (set! resolved (cons to-resolve resolved)) ; ...add it to the set of processed rules,...
                 (set! to-check (remq to-resolve to-check)) ; ...remove it from the set of rules to process and...
                 (loop)))))))) ; ...recur.
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                   Support API                                                                  ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; Given an AST, an association list L of attribute pretty-printers and an output port, print a human-readable ASCII representation of
 ; the AST on the output port. The elements of the association list L are (attribute-name pretty-printing-function) pairs. Every attribute
 ; for which L contains an entry is printed when the AST node it is associated with is printed. Thereby, the given pretty printing function
 ; is applied to the attribute's value before printing it. Beware: The output port is never closed by this function --- neither in case of
 ; an io-exception nor after finishing printing the AST.
 (define print-ast
   (lambda (ast attribute-pretty-printer-list output-port)
     (letrec ((print-indentation
               (lambda (n)
                 (if (> n 0)
                     (begin
                       (print-indentation (- n 1))
                       (my-display " |"))
                     (my-display #\newline))))
              (my-display
               (lambda (to-display)
                 (display to-display output-port))))
       (let loop ((ast-depth 0)
                  (ast ast))
         (cond
           ((ast-list-node? ast) ; Print list nodes
            (print-indentation ast-depth)
            (print-indentation ast-depth)
            (my-display "-* ")
            (my-display
             (symbol->string
              (vector-ref
               (list-ref
                (ast-rule-production (node-ast-rule (node-parent ast)))
                (ast-child-index ast))
               0)))
            (for-each
             (lambda (element)
               (loop (+ ast-depth 1) element))
             (node-children ast)))
           ((node-non-terminal? ast) ; Print non-terminal
            (print-indentation ast-depth)
            (print-indentation ast-depth)
            (my-display "-\\ ")
            (my-display (symbol->string (ast-node-type ast)))
            (for-each
             (lambda (att)
               (let* ((name (attribute-definition-name (attribute-instance-definition att)))
                      (pretty-printer-entry (assq name attribute-pretty-printer-list)))
                 (if pretty-printer-entry
                     (begin
                       (print-indentation (+ ast-depth 1))
                       (my-display " <")
                       (my-display (symbol->string name))
                       (my-display "> ")
                       (my-display ((cdr pretty-printer-entry) (att-value name ast)))))))
             (node-attributes ast))
            (for-each
             (lambda (child)
               (loop (+ ast-depth 1) child))
             (node-children ast)))
           (else ; Print terminal
            (print-indentation ast-depth)
            (my-display "- ")
            (my-display (node-children ast)))))
       (my-display #\newline))))
 
 ; Syntax definition which eases the use of common RACR library functions by providing an environment where mandatory RACR specification
 ; parameters are already bound to a given specification.
 (define-syntax with-specification
   (lambda (x)
     (syntax-case x ()
       ((k spec body ...)
        #`(let* ((spec* spec)
                 (#,(datum->syntax #'k 'ast-rule)
                  (lambda (rule)
                    (specify-ast-rule spec* rule)))
                 (#,(datum->syntax #'k 'compile-ast-specifications)
                  (lambda (start-symbol)
                    (compile-ast-specifications spec* start-symbol)))
                 (#,(datum->syntax #'k 'compile-ag-specifications)
                  (lambda ()
                    (compile-ag-specifications spec*)))
                 (#,(datum->syntax #'k 'create-ast)
                  (lambda (rule children)
                    (create-ast spec* rule children)))
                 (#,(datum->syntax #'k 'specification-phase)
                  (lambda ()
                    (racr-specification-specification-phase spec*)))
                 (#,(datum->syntax #'k 'specify-attribute)
                  (lambda (attribute-name non-terminal context-name-or-position cached? equation circularity-definition)
                    (specify-attribute spec* attribute-name non-terminal context-name-or-position cached? equation circularity-definition))))
            (let-syntax ((#,(datum->syntax #'k 'ag-rule)
                          (syntax-rules ()
                            ((_ attribute-name definition (... ...))
                             (specify-ag-rule spec* attribute-name definition (... ...))))))
              body ...))))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                         Abstract Syntax Tree Annotations                                                       ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; Given an AST fragment, a node type T and an annotation name and value, add to each node of type T of the fragment the given
 ; annotation, iff it does not yet have an equally named one. An exception is thrown, if any attributes of the AST the given
 ; node is part of are in evaluation.
 (define ast-weave-annotations
   (lambda (node type name value)
     (if (evaluator-state-in-evaluation? (node-evaluator-state node))
         (throw-exception "Cannot weave " name " annotation; There are attributes in evaluation."))
     (if (not (ast-annotation? node name))
         (cond
           ((and (not (ast-list-node? node)) (ast-subtype? node type))
            (ast-annotation-set! node name value))
           ((and (ast-list-node? node) (eq? type 'list-node))
            (ast-annotation-set! node name value))))
     (for-each
      (lambda (child)
        (if (not (node-terminal? child))
            (ast-weave-annotations child type name value)))
      (node-children node))))
 
 ; Given a node and an annotation name, return whether the node has an annotation with the given name or not. An exception is
 ; thrown, if any attributes of the AST the given node is part of are in evaluation.
 (define ast-annotation?
   (lambda (node name)
     (if (evaluator-state-in-evaluation? (node-evaluator-state node))
         (throw-exception "Cannot check for " name " annotation; There are attributes in evaluation."))
     (assq name (node-annotations node))))
 
 ; Given a node return the value of a certain annotation associated with it. An exception is thrown, if the node has no such
 ; annotation or any attributes of the AST it is part of are in evaluation.
 (define ast-annotation
   (lambda (node name)
     (if (evaluator-state-in-evaluation? (node-evaluator-state node))
         (throw-exception "Cannot access " name " annotation; There are attributes in evaluation."))
     (let ((annotation (ast-annotation? node name)))
       (if annotation
           (cdr annotation)
           (throw-exception "Cannot access " name " annotation; The given node has no such annotation.")))))
 
 ; Given a node and an annotation name N and value V, add an annotation with name N and value V to the node. If the node already has
 ; an annotation named N, set its value to V. If V is a procedure, the annotation's value is a procedure P calling V with the node
 ; the annotation is associated with as first argument and arbitrary many further given arguments. An exception is thrown, if any
 ; attributes of the AST the given node is part of are in evaluation.
 (define ast-annotation-set!
   (lambda (node name value)
     (if (evaluator-state-in-evaluation? (node-evaluator-state node))
         (throw-exception "Cannot set " name " annotation; There are attributes in evaluation."))
     (if (not (symbol? name))
         (throw-exception "Cannot set " name " annotation; Annotation names must be Scheme symbols."))
     (let ((annotation (ast-annotation? node name))
           (value
            (if (procedure? value)
                (lambda args
                  (apply value node args))
                value)))
       (if annotation
           (set-cdr! annotation value)
           (node-annotations-set! node (cons (cons name value) (node-annotations node)))))))
 
 ; Given a node and an annotation name, remove any equally named annotation associated with the node. An exception is thrown,
 ; if any attributes of the AST the given node is part of are in evaluation.
 (define ast-annotation-remove!
   (lambda (node name)
     (if (evaluator-state-in-evaluation? (node-evaluator-state node))
         (throw-exception "Cannot remove " name " annotation; There are attributes in evaluation."))
     (node-annotations-set!
      node
      (remp
       (lambda (entry)
         (eq? (car entry) name))
       (node-annotations node)))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                       Abstract Syntax Tree Specifications                                                      ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; Calling this function adds to the given RACR specification the AST rule encoded in the given symbol. To this end, the given symbol is
 ; parsed as AST rule and internally stored. The function aborts with an exception, iff the given symbol encodes no valid rule or
 ; there already exists a definition for the rule's l-hand. Beware, the rule's super- and sub-types and production symbols still
 ; have to be resolved later.
 (define specify-ast-rule
   (lambda (spec rule)
     ;;; Ensure, that the RACR system is in the correct specification phase:
     (if (> (racr-specification-specification-phase spec) 1)
         (throw-exception "Unexpected AST rule " rule "; AST rules can only be defined in the AST specification phase."))
     
     (let* (; Support function, that given a symbol S encoding an AST rule, parses S and returns an appropriate rule representation.
            (parse-rule
             (lambda (rule)
               (letrec ((rule-string (symbol->string rule)) ; String representation of the rule encoded in the given symbol; Used for parsing
                        (pos 0) ; The current parsing position
                        ; Support function returning, whether the end of the parsing string is reached or not:
                        (eos?
                         (lambda ()
                           (= pos (string-length rule-string))))
                        ; Support function returning the current character to parse:
                        (my-peek-char
                         (lambda ()
                           (string-ref rule-string pos)))
                        ; Support function returning the current character to parse and incrementing the parsing position:
                        (my-read-char
                         (lambda ()
                           (let ((c (my-peek-char)))
                             (set! pos (+ pos 1))
                             c)))
                        ; Support function matching a certain character:
                        (match-char!
                         (lambda (c)
                           (if (eos?)
                               (throw-exception "Unexpected end of AST rule " rule "; Expected " c " character.")
                               (if (char=? (my-peek-char) c)
                                   (set! pos (+ pos 1))
                                   (throw-exception "Invalid AST rule " rule "; Unexpected " (my-peek-char) " character.")))))
                        ; Support function parsing a symbol, i.e., retrieving its name, type, if it is a list and optional context-name.
                        ; It returns a (name-as-scheme-symbol terminal? klenee? context-name-as-scheme-symbol?) quadrupel:
                        (parse-symbol
                         (lambda (type location) ; type: non-terminal, symbol; location: l-hand, r-hand 
                           (if (eos?)
                               (throw-exception "Unexpected end of AST rule " rule "; Expected " type "."))
                           (let* ((terminal? (char-lower-case? (my-peek-char)))
                                  (name
                                   (let loop ((chars (list)))
                                     (if (and (not (eos?)) (char-alphabetic? (my-peek-char)))
                                         (begin
                                           (if (and terminal? (not (char-lower-case? (my-peek-char))))
                                               (throw-exception "Invalid AST rule " rule "; Unexpected " (my-peek-char) " character."))
                                           (loop (cons (my-read-char) chars)))
                                         (reverse chars)))))
                             (if (null? name)
                                 (throw-exception "Unexpected " (my-peek-char) " character in AST rule " rule "; Expected " type "."))
                             (let* ((klenee? (and (not terminal?) (eq? location 'r-hand) (not (eos?)) (char=? (my-peek-char) #\*) (my-read-char)))
                                    (context-name?
                                     (and
                                      (not terminal?)
                                      (eq? location 'r-hand)
                                      (not (eos?))
                                      (char=? (my-peek-char) #\<)
                                      (my-read-char)
                                      (append
                                       (let loop ((chars (list)))
                                         (if (and (not (eos?)) (char-alphabetic? (my-peek-char)))
                                             (loop (cons (my-read-char) chars))
                                             (reverse chars)))
                                       (let loop ((chars (list)))
                                         (if (and (not (eos?)) (char-numeric? (my-peek-char)))
                                             (loop (cons (my-read-char) chars))
                                             (reverse chars)))))))
                               (if (and (eq? type 'non-terminal) terminal?)
                                   (throw-exception "Unexpected terminal in AST rule " rule "; Left hand side symbols must be non-terminals."))
                               (if context-name?
                                   (if (null? context-name?)
                                       (throw-exception "Invalid AST rule " rule "; Missing context-name.")
                                       (if (not (char-alphabetic? (car context-name?)))
                                           (throw-exception "Malformed context-name in AST rule " rule "; Context-names must start with a letter."))))
                               (let* ((name-string (list->string name))
                                      (name-symbol (string->symbol name-string)))
                                 (vector
                                  name-symbol
                                  (not terminal?)
                                  klenee?
                                  (if context-name?
                                      (string->symbol (list->string context-name?))
                                      (if klenee?
                                          (string->symbol (string-append name-string "*"))
                                          name-symbol)))))))))
                 (let* ((l-hand (parse-symbol 'non-terminal 'l-hand)); The rule's l-hand
                        (supertype ; The rule's super-type
                         (and (not (eos?)) (char=? (my-peek-char) #\:) (my-read-char) (vector-ref (parse-symbol 'non-terminal 'l-hand) 0))))
                   (match-char! #\-)
                   (match-char! #\>)
                   (make-ast-rule
                    spec
                    rule
                    (append
                     (list l-hand)
                     (let loop ((r-hand
                                 (if (not (eos?))
                                     (list (parse-symbol 'symbol 'r-hand))
                                     (list))))
                       (if (eos?)
                           (reverse r-hand)
                           (begin
                             (match-char! #\-)
                             (loop (cons (parse-symbol 'symbol 'r-hand) r-hand))))))
                    supertype
                    (list)
                    (list))))))
            (rule* (parse-rule rule)) ; Representation of the parsed rule
            (l-hand (vector-ref (car (ast-rule-production rule*)) 0))) ; The rule's l-hand
       (if (hashtable-contains? (racr-specification-rules spec) l-hand) ; Check, that the rule's l-hand is not already defined.
           (throw-exception "Invalid AST rule " rule "; Redefinition of " l-hand "."))
       (hashtable-set! (racr-specification-rules spec) l-hand rule*)))) ; Add the rule to the RACR system.
 
 ; Calling this function finishes the AST specification phase of the given RACR specification, whereby the given symbol becomes the start
 ; symbol. The AST specification is checked for completeness and correctness, i.e., (1) all non-terminals are defined, (2) rule inheritance
 ; is cycle-free, (3) the start symbol is defined, (4) the start symbol is start separated, (5) no non-terminal inherits from the start
 ; symbol, (6) the start symbol does not inherit from any non-terminal and (7) all non-terminals are reachable and (8) productive. Further,
 ; it is ensured, that (9) for every rule the context-names of its children are unique. In case of any violation, an exception is thrown.
 ; Further, super-types and production symbols are resolved and inherited production symbols are added to their respective heirs.
 (define compile-ast-specifications
   (lambda (spec start-symbol)
     ;;; Ensure, that the RACR system is in the correct specification phase and...
     (let ((current-phase (racr-specification-specification-phase spec)))
       (if (> current-phase 1)
           (throw-exception "Unexpected AST compilation; The AST specifications already have been compiled.")
           (racr-specification-specification-phase-set! spec (+ current-phase 1)))) ; ...iff so proceed to the next specification phase.
     
     (racr-specification-start-symbol-set! spec start-symbol)
     (let* ((rules-table (racr-specification-rules spec)) ; Rules hashtable
            (rules-list ; List of all rules
             (call-with-values
              (lambda () (hashtable-entries rules-table))
              (lambda (key-vector value-vector)
                (vector->list value-vector))))
            ; Support function, that given a rule R returns a list of all rules directly derivable from R:
            (derivable-rules
             (lambda (rule*)
               (fold-left
                (lambda (result symb*)
                  (if (vector-ref symb* 1)
                      (append result (list (vector-ref symb* 1)) (ast-rule-subtypes (vector-ref symb* 1)))
                      result))
                (list)
                (cdr (ast-rule-production rule*))))))
       
       ;;; Resolve supertypes and non-terminals occuring in productions and ensure all non-terminals are defined:
       (for-each
        (lambda (rule*)
          (if (ast-rule-supertype rule*)
              (let ((supertype-entry (hashtable-ref rules-table (ast-rule-supertype rule*) #f)))
                (if (not supertype-entry)
                    (throw-exception "Invalid AST rule " (ast-rule-as-symbol rule*) "; The supertype " (ast-rule-supertype rule*) " is not defined.")
                    (ast-rule-supertype-set! rule* supertype-entry))))
          (for-each
           (lambda (symb*)
             (if (vector-ref symb* 1)
                 (let ((symb-definition (hashtable-ref rules-table (vector-ref symb* 0) #f)))
                   (if (not symb-definition)
                       (throw-exception "Invalid AST rule " (ast-rule-as-symbol rule*) "; Non-terminal " (vector-ref symb* 0) " is not defined."))
                   (vector-set! symb* 1 symb-definition))))
           (cdr (ast-rule-production rule*))))
        rules-list)
       
       ;;; Resolve rules' sub-types:
       (for-each
        (lambda (rule1*)
          (let ((subtypes (list))
                (change? #f))
            (let loop ()
              (set! change? #f)
              (for-each
               (lambda (rule*)
                 (let ((supertype (ast-rule-supertype rule*)))
                   (if (and
                        supertype
                        (or (eq? supertype rule1*) (memq supertype subtypes))
                        (not (memq rule* subtypes)))
                       (begin
                         (set! subtypes (cons rule* subtypes))
                         (set! change? #t)))))
               rules-list)
              (if change?
                  (loop)))
            (ast-rule-subtypes-set! rule1* subtypes)))
        rules-list)
       
       ;;; Ensure, that inheritance is cycle-free:
       (for-each
        (lambda (rule*)
          (if (memq rule* (ast-rule-subtypes rule*))
              (throw-exception "Invalid AST grammar; The definition of " (ast-rule-as-symbol rule*) " depends on itself (cyclic inheritance).")))
        rules-list)
       
       ;;; Ensure, that the start symbol is defined:
       (if (not (hashtable-contains? rules-table start-symbol))
           (throw-exception "Invalid AST grammar; The start symbol " start-symbol " is not defined."))
       
       ;;; Ensure, that the start symbol has no super- and sub-type:
       (let ((supertype (ast-rule-supertype (hashtable-ref rules-table start-symbol #f))))
         (if supertype
             (throw-exception "Invalid AST grammar; The start symbol " start-symbol " inherits from " (ast-rule-as-symbol supertype) ".")))
       (let ((subtypes (ast-rule-subtypes (hashtable-ref rules-table start-symbol #f))))
         (if (not (null? subtypes))
             (throw-exception
              "Invalid AST grammar; The rules "
              (map ast-rule-as-symbol subtypes)
              " inherit from the start symbol "
              start-symbol
              ".")))
       
       ;;; Ensure, that the CFG is start separated:
       (let ((start-rule (hashtable-ref rules-table start-symbol #f)))
         (for-each
          (lambda (rule*)
            (if (memq start-rule (derivable-rules rule*))
                (throw-exception
                 "Invalid AST grammar; The start symbol "
                 start-symbol
                 " is not start separated because of rule "
                 (ast-rule-as-symbol rule*)
                 ".")))
          rules-list))
       
       ;;; Resolve inherited production symbols:
       (apply-wrt-ast-inheritance
        (lambda (rule*)
          (ast-rule-production-set!
           rule*
           (append
            (list (car (ast-rule-production rule*)))
            (cdr (ast-rule-production (ast-rule-supertype rule*)))
            (cdr (ast-rule-production rule*)))))
        rules-list)
       
       ;;; Ensure context-names are unique:
       (for-each
        (lambda (rule*)
          (let loop ((rest-production (cdr (ast-rule-production rule*))))
            (if (not (null? rest-production))
                (let ((current-context-name (vector-ref (car rest-production) 3)))
                  (if (find
                       (lambda (symb*)
                         (eq? (vector-ref symb* 3) current-context-name))
                       (cdr rest-production))
                      (throw-exception
                       "Invalid AST grammar; The context-name "
                       current-context-name
                       " is not unique for rule "
                       (ast-rule-as-symbol rule*)
                       "."))
                  (loop (cdr rest-production))))))
        rules-list)
       
       ;;; Ensure, that all non-terminals can be derived from the start symbol:
       (let* ((to-check (list (hashtable-ref rules-table start-symbol #f)))
              (checked (list)))
         (let loop ()
           (if (> (length to-check) 0)
               (let ((rule* (car to-check)))
                 (set! to-check (cdr to-check))
                 (set! checked (cons rule* checked))
                 (for-each
                  (lambda (derivable-rule)
                    (if (and
                         (not (memq derivable-rule checked))
                         (not (memq derivable-rule to-check)))
                        (set! to-check (cons derivable-rule to-check))))
                  (derivable-rules rule*))
                 (loop))))
         (let ((non-derivable-rules
                (filter
                 (lambda (rule*)
                   (not (memq rule* checked)))
                 rules-list)))
           (if (> (length non-derivable-rules) 0)
               (throw-exception "Invalid AST grammar; The rules " (map ast-rule-as-symbol non-derivable-rules) " cannot be derived."))))
       
       ;;; Ensure, that all non-terminals are productive:
       (let* ((productive-rules (list))
              (to-check rules-list)
              (productive-rule?
               (lambda (rule*)
                 (not (find
                       (lambda (symb*)
                         (and
                          (vector-ref symb* 1)
                          (not (memq (vector-ref symb* 1) productive-rules))))
                       (cdr (ast-rule-production rule*)))))))
         (let loop ()
           (let ((productive-rule
                  (find productive-rule? to-check)))
             (if productive-rule
                 (begin
                   (set! to-check (remq productive-rule to-check))
                   (set! productive-rules (cons productive-rule productive-rules))
                   (loop)))))
         (if (> (length to-check) 0)
             (throw-exception "Invalid AST grammar; The rules " (map ast-rule-as-symbol to-check) " are not productive."))))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                       Attribute Grammar Specifications                                                         ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; Syntax definition which eases the specification of attributes by:
 ;  1) Permitting the specification of arbitrary many definitions for a certain attribute for different contexts without the need to repeat
 ;     the attribute name several times
 ;  2) Automatic quoting of attribute names (thus, the given name must be an ordinary identifier)
 ;  3) Automatic quoting of non-terminals and context-names (thus, contexts must be ordinary identifiers)
 ;  4) Optional caching and circularity information (by default caching is enabled and attribute definitions are non-circular)
 ;  5) Context-names of synthesized attribute definitions can be left
 ; Note, that (3) prohibits the specification of contexts using positions within productions. For details about attribute
 ; specifications see "specify-attribute".
 (define-syntax specify-ag-rule
   (lambda (x)
     (syntax-case x ()
       ((_ spec attribute-name definition ...)
        (and (identifier? #'attribute-name) (not (null? #'(definition ...))))
        #'(let ((spec* spec)
                (attribute-name* 'attribute-name))
            (let-syntax
                ((specify-attribute*
                  (syntax-rules ()
                    ((_ spec* attribute-name* ((non-terminal position) equation))
                     (specify-attribute spec* attribute-name* 'non-terminal 'position #t equation #f))
                    ((_ spec* attribute-name* ((non-terminal position) cached? equation))
                     (specify-attribute spec* attribute-name* 'non-terminal 'position cached? equation #f))
                    ((_ spec* attribute-name* ((non-terminal position) equation bottom-value equivalence-function))
                     (specify-attribute spec* attribute-name* 'non-terminal 'position #t equation (cons bottom-value equivalence-function)))
                    ((_ spec* attribute-name* ((non-terminal position) cached? equation bottom-value equivalence-function))
                     (specify-attribute spec* attribute-name* 'non-terminal 'position cached? equation (cons bottom-value equivalence-function)))
                    ((_ spec* attribute-name* (non-terminal equation))
                     (specify-attribute spec* attribute-name* 'non-terminal 0 #t equation #f))
                    ((_ spec* attribute-name* (non-terminal cached? equation))
                     (specify-attribute spec* attribute-name* 'non-terminal 0 cached? equation #f))
                    ((_ spec* attribute-name* (non-terminal equation bottom-value equivalence-function))
                     (specify-attribute spec* attribute-name* 'non-terminal 0 #t equation (cons bottom-value equivalence-function)))
                    ((_ spec* attribute-name* (non-terminal cached? equation bottom-value equivalence-function))
                     (specify-attribute spec* attribute-name* 'non-terminal 0 cached? equation (cons bottom-value equivalence-function))))))
              (specify-attribute* spec* attribute-name* definition) ...))))))
 
 ; Calling this function adds to the given RACR specification the given attribute definition. To this end, the given definition's
 ; representation is internally stored, after checking, that (1) it is properly encoded (syntax check), (2) its context is defined,
 ; (3) the context is a non-terminal and (4) the definition is unique (no redefinition error). In case of any violation, an exception
 ; is thrown. The context of an attribute definition can be given by either, a context-name denoting a certain child of a non-terminal
 ; (inherited attribute definition), the symbol '* denoting a synthesized attribute definition or a position within the production
 ; of a non-terminal (position > 0: inherited attribute definition | position = 0: synthesized attribute definition).
 (define specify-attribute
   (lambda (spec attribute-name non-terminal context-name-or-position cached? equation circularity-definition)
     ;;; Ensure correct argument types:
     (let ((wrong-argument-type
            (or
             (and (not (symbol? attribute-name))
                  "Attribute name : symbol")
             (and (not (symbol? non-terminal))
                  "AST rule : non-terminal")
             (and (not (symbol? context-name-or-position))
                  (or (not (integer? context-name-or-position)) (< context-name-or-position 0))
                  "Production position : index or context-name")
             (and (not (procedure? equation))
                  "Attribute equation : function")
             (and circularity-definition
                  (not (pair? circularity-definition))
                  (not (procedure? (cdr circularity-definition)))
                  "Circularity definition : #f or (bottom-value equivalence-function) pair"))))
       (if wrong-argument-type
           (throw-exception "Invalid attribute definition; Wrong argument type (" wrong-argument-type ").")))
     
     ;;; Ensure, that the RACR system is in the correct specification phase:
     (if (not (= (racr-specification-specification-phase spec) 2))
         (throw-exception "Unexpected " attribute-name " attribute definition; Attributes can only be defined in the AG specification phase."))
     
     (let ((ast-rule (hashtable-ref (racr-specification-rules spec) non-terminal #f)))
       ;;; Ensure, that the given AST rule is defined:
       (if (not ast-rule)
           (throw-exception "Invalid attribute definition; The non-terminal " non-terminal " is not defined."))
       
       (let ((position ;;; Ensure, that the given context exists:
              (if (symbol? context-name-or-position)
                  (if (eq? context-name-or-position '*)
                      0
                      (let loop ((pos 1)
                                 (rest-production (cdr (ast-rule-production ast-rule))))
                        (if (null? rest-production)
                            (throw-exception
                             "Invalid attribute definition; The non-terminal "
                             non-terminal
                             " has no "
                             context-name-or-position
                             " context.")
                            (if (eq? (vector-ref (car rest-production) 3) context-name-or-position)
                                pos
                                (loop (+ pos 1) (cdr rest-production))))))
                  (if (>= context-name-or-position (length (ast-rule-production ast-rule)))
                      (throw-exception
                       "Invalid attribute definition; There exists no "
                       context-name-or-position
                       "'th position in the context of "
                       non-terminal
                       "].")
                      context-name-or-position))))
         
         ;;; Ensure, that the given context is a non-terminal:
         (if (not (vector-ref (list-ref (ast-rule-production ast-rule) position) 1))
             (throw-exception
              "Invalid attribute definition; The "
              position
              "'th position in the context of "
              non-terminal
              " is a terminal but attributes can only be defined for non-terminals."))
         
         ;;; Ensure, that the attribute is not already defined for the given context:
         (if (find
              (lambda (def)
                (and (eq? attribute-name (attribute-definition-name def)) (= position (cdr (attribute-definition-context def)))))
              (ast-rule-attributes ast-rule))
             (throw-exception "Invalid attribute definition; Redefinition of " attribute-name " for context " non-terminal position "."))
         
         ;;; Add the definition to the AST rule:
         (ast-rule-attributes-set!
          ast-rule
          (cons
           (make-attribute-definition
            attribute-name
            (cons ast-rule position)
            equation
            circularity-definition
            cached?)
           (ast-rule-attributes ast-rule)))))))
 
 ; Calling this function finishes the given RACR specification's AG specification. Thereby, inherited attributes are added to their
 ; respective heirs iff they are not shadowed.
 (define compile-ag-specifications
   (lambda (spec)
     ;;; Ensure, that the RACR system is in the correct specification phase and...
     (let ((current-phase (racr-specification-specification-phase spec)))
       (if (< current-phase 2)
           (throw-exception "Unexpected AG compilation. The AST specifications are not yet compiled."))
       (if (> current-phase 2)
           (throw-exception "Unexpected AG compilation. The AG specifications already have been compiled.")
           (racr-specification-specification-phase-set! spec (+ current-phase 1)))) ; ...iff so proceed to the next specification phase.
     
     (let* ((ast-rules ; List of all AST rules
             (call-with-values
              (lambda () (hashtable-entries (racr-specification-rules spec)))
              (lambda (key-vector value-vector)
                (vector->list value-vector)))))
       ;;; Resolve attribute definitions inherited from a super-type. Thus,...
       (apply-wrt-ast-inheritance ; ...for every AST rule R which has a super-type,...
        (lambda (ast-rule*)
          (for-each ; ...check for every attribute definition of R's super-type...
           (lambda (super-def)
             (if (not (find ; ...if it is shadowed by an attribute definition of R....
                       (lambda (sub-def)
                         (and
                          (eq? (attribute-definition-name super-def) (attribute-definition-name sub-def))
                          (= (cdr (attribute-definition-context super-def)) (cdr (attribute-definition-context sub-def)))))
                       (ast-rule-attributes ast-rule*)))
                 ; ...If the super-type's attribute definition is not shadowed...
                 (ast-rule-attributes-set! ; ...add...
                  ast-rule*
                  (cons
                   (make-attribute-definition ; ...a copy of the attribute definition inherited...
                    (attribute-definition-name super-def)
                    (cons ast-rule* (cdr (attribute-definition-context super-def))) ; ...to R.
                    (attribute-definition-equation super-def)
                    (attribute-definition-circularity-definition super-def)
                    (attribute-definition-cached? super-def))
                   (ast-rule-attributes ast-rule*)))))
           (ast-rule-attributes (ast-rule-supertype ast-rule*))))
        ast-rules))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                             Attribute Evaluator                                                                ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; INTERNAL FUNCTION: Given a node find a certain attribute associated with it. Iff the node has no
 ; such attribute, return #f, otherwise the attribute.
 (define find-attribute
   (lambda (name n)
     (find
      (lambda (att)
        (eq? (attribute-definition-name (attribute-instance-definition att)) name))
      (node-attributes n))))
 
 ; INTERNAL FUNCTION: Given a node N find a certain attribute associated with it, whereas in case no proper attribute
 ; is associated with N itself the search is extended to find a broadcast solution. Iff the extended search finds a
 ; solution, appropriate copy propergation attributes (i.e., broadcasters) are added. Iff no attribute instance can be
 ; found, an exception is thrown. Otherwise, the attribute or its respective last broadcaster is returned.
 (define lookup-attribute
   (lambda (name n)
     (let loop ((n n)) ; Recursively...
       (let ((att (find-attribute name n))) ; ...check if the current node has a proper attribute instance....
         (if att
             att ; ...Iff it has, return the found defining attribute instance.
             (let ((parent (node-parent n))) ; ...Iff no defining attribute instance can be found...
               (if (not parent) ; ...check if there exists a parent node that may provide a definition....
                   (throw-exception ; ...Iff not, thrown an exception,...
                    "AG evaluator exception; Cannot access unknown attribute " name ".")
                   (let* ((att (loop parent)) ; ...otherwise proceed the search at the parent node. Iff it succeeds...
                          (find-child-index
                           (lambda (n)
                             (let loop ((children (node-children (node-parent n)))
                                        (pos 0))
                               (if (eq? (car children) n)
                                   (+ pos 1)
                                   (loop (cdr children) (+ pos 1))))))
                          (broadcaster ; ...construct a broadcasting attribute instance...
                           (make-attribute-instance
                            (make-attribute-definition ; ...whose...
                             name
                             (if (eq? (node-ast-rule parent) 'list-node) ; ...definition context depends if the parent node is a list-node or not....
                                 (cons ; ...Iff it is a list-node the broadcaster's context is...
                                  (node-ast-rule (node-parent parent)) ; ...the list-node's parent node and...
                                  (find-child-index parent)) ; ...child position.
                                 (cons ; ...Iff the parent node is not a list-node the broadcaster's context is...
                                  (node-ast-rule parent) ; ...the parent node and...
                                  (find-child-index n))) ; ...the current node's child position. Further,...
                             (lambda (n . args) ; ...the broadcaster's equation just calls the parent node's counterpart. Finally,...
                               (apply att-value name (ast-parent n) args))
                             (attribute-definition-circularity-definition (attribute-instance-definition att))
                             #f)
                            n)))
                     (node-attributes-set! n (cons broadcaster (node-attributes n))) ; ...add the constructed broadcaster and...
                     broadcaster)))))))) ; ...return it as the current node's look-up result.
 
 ; Given a node return the value of one of its attributes A. If required, A is evaluated, whereas all its
 ; meta-information like dependencies etc. are properly computed.
 ; DEPENENCIES:
 ;  - Iff another attribute A2 already is in evaluation: A2 depends on A
 (define att-value
   (lambda (name n . args)
     (let* (; The evaluator state used and changed throughout evaluation:
            (evaluator-state (node-evaluator-state n))
            ; The attribute instance to evaluate:
            (att (lookup-attribute name n))
            ; The attribute's definition:
            (att-def (attribute-instance-definition att))
            ; The attribute's value cache entry for the given arguments:
            (vc-hit
             (if (attribute-definition-cached? att-def)
                 (hashtable-ref (attribute-instance-value-cache att) args racr-nil)
                 racr-nil)))
       (if (not (eq? vc-hit racr-nil)) ; First, check if the attribute's value is cached....
           (begin ; ...Iff it is,...
             ; maintaine attribute dependencies, i.e., iff this attribute is evaluated throughout the evaluation
             ; of another attribute, the other attribute depends on this one. Afterwards,...
             (add-dependency:att->att att)        
             vc-hit) ; ...return the attribute's cached value.
           ; ...Iff the attribute is not cached it must be evaluated. Therefore, prepare a few support values and functions:
           (let* (; The attribute's computed value to return:
                  (result racr-nil)
                  ; The attribute's cycle cache entry for the given arguments:
                  (cc-hit (hashtable-ref (attribute-instance-cycle-cache att) args #f))
                  ; Boolean value; #t iff the attribute already is in evaluation for the given arguments:
                  (entered? (and cc-hit (cdr cc-hit)))
                  ; Boolean value; #t iff the attribute is declared to be circular:
                  (circular? (attribute-definition-circular? att-def))
                  ; Boolean value; #t iff the attribute is declared to be circular and is the starting point for a
                  ; fix-point evaluation:
                  (start-fixpoint-computation? (and circular? (not (evaluator-state-ag-in-cycle? evaluator-state))))
                  ; Support function that checks if the attribute's value changed throughout fix-point evaluation and
                  ; updates its and the evaluator's state accordingly:
                  (update-cycle-cache
                   (lambda ()
                     (attribute-instance-args-cache-set! att args)
                     (if (not
                          ((cdr (attribute-definition-circularity-definition att-def))
                           result
                           (car cc-hit)))
                         (begin
                           (set-car! cc-hit result)
                           (evaluator-state-ag-cycle-change?-set! evaluator-state #t))))))
             ; Now, decide how to evaluate the attribute dependening on whether the attribute is circular, already in evaluation
             ; or starting point for a fix-point evaluation:
             (cond
               (start-fixpoint-computation? ; EVALUATION-CASE (1): Circular attribute starting point for a fix-point evaluation.
                (let (; Flag indicating abnormal termination of the fix-point evaluation (e.g., by implementation
                      ; errors within applied attribute equations and respective exceptions or the application of
                      ; a continuation outside the fix-point evaluation's scope):
                      (abnormal-termination? #t))
                  (dynamic-wind
                   (lambda ()
                     ; Maintaine attribute dependencies, i.e., iff this attribute is evaluated throughout the evaluation
                     ; of another attribute, the other attribute depends on this one and...
                     (add-dependency:att->att att)
                     ; ...this attribute must depend on any other attributes that will be evaluated through its own evaluation. Further,..
                     (evaluator-state-att-eval-stack-set! evaluator-state (cons att (evaluator-state-att-eval-stack evaluator-state)))
                     ; ...update the evaluator state that we are about to start a fix-point evaluation and...
                     (evaluator-state-ag-in-cycle?-set! evaluator-state #t)
                     ; ...mark, that the attribute is in evaluation and construct an appropriate cycle-cache entry.
                     (set! cc-hit (cons (car (attribute-definition-circularity-definition att-def)) #t))
                     (hashtable-set! (attribute-instance-cycle-cache att) args cc-hit))
                   (lambda ()
                     (let loop () ; Start fix-point evaluation. Thus, as long as...
                       (evaluator-state-ag-cycle-change?-set! evaluator-state #f) ; ...an attribute's value changes...
                       (set! result (apply (attribute-definition-equation att-def) n args)) ; ...evaluate the attribute,...
                       (update-cycle-cache) ; ...update its cycle cache and...
                       ; ...check if throughout its evaluation the value of any attribute it depends on changed....
                       (if (evaluator-state-ag-cycle-change? evaluator-state) ; ...Iff a value changed,
                           (loop)) ; ...trigger the attribute's evaluation once more, until a fix-point is reached. Finally,...
                       (set! abnormal-termination? #f))) ; ...indicate that the fix-point evaluation terminated normal.
                   (lambda ()
                     ; Mark that the fix-point evaluation is finished and...
                     (evaluator-state-ag-in-cycle?-set! evaluator-state #f)
                     ; ...update the caches of all circular attributes evaluated throughout it. To do so,...
                     (let loop ((att att))
                       (if (not (attribute-definition-circular? (attribute-instance-definition att)))
                           ; ...ignore non-circular attributes and just proceed with the attributes they depend on (to
                           ; ensure all strongly connected components within a weakly connected one are updated)....
                           (for-each
                            loop
                            (attribute-instance-attribute-dependencies att))
                           (if (> (hashtable-size (attribute-instance-cycle-cache att)) 0) ; ...In case of circular attributes not yet updated,...
                               (begin
                                 (if (and ; ...check...
                                      (not abnormal-termination?) ; ...if the fix-point evaluation terminated normal and...
                                      (attribute-definition-cached? (attribute-instance-definition att))) ; ...caching is enabled,...
                                     (hashtable-set! ; ...iff so...
                                      (attribute-instance-value-cache att) ; ...each such attribute's fix-point value to cache...
                                      (attribute-instance-args-cache att) ; ...is the value computed throughout its last invocation. Further,...
                                      (car (hashtable-ref (attribute-instance-cycle-cache att) (attribute-instance-args-cache att) #f))))
                                 (hashtable-clear! (attribute-instance-cycle-cache att)) ; ...ALWAYS clear the attribute's cycle and...
                                 (attribute-instance-args-cache-set! att racr-nil) ; ...most recent arguments cache....
                                 (for-each ; ...Then proceed with the attributes the circular attribute depends on....
                                  loop
                                  (attribute-instance-attribute-dependencies att))))))
                     ; ...Finally, pop the attribute from the attribute evaluation stack.
                     (evaluator-state-att-eval-stack-set! evaluator-state (cdr (evaluator-state-att-eval-stack evaluator-state)))))))
               
               ((and circular? entered?) ; EVALUATION-CASE (2): Circular attribute, already in evaluation for the given arguments.
                ; Maintaine attribute dependencies, i.e., the other attribute throughout whose evaluation
                ; this attribute is evaluated must depend on this one. Finally,...
                (add-dependency:att->att att)
                ; ...the result is the attribute's cycle cache entry.
                (set! result (car cc-hit)))
               
               (circular? ; EVALUATION-CASE (3): Circular attribute not in evaluation and entered throughout a fix-point evaluation.
                (dynamic-wind
                 (lambda ()
                   ; Maintaine attribute dependencies, i.e., iff this attribute is evaluated throughout the evaluation
                   ; of another attribute, the other attribute depends on this one and...
                   (add-dependency:att->att att)
                   ; ...this attribute must depend on any other attributes that will be evaluated through its own evaluation. Further,..
                   (evaluator-state-att-eval-stack-set! evaluator-state (cons att (evaluator-state-att-eval-stack evaluator-state)))
                   ; ...mark, that the attribute is in evaluation and construct an appropriate cycle-cache entry if required.
                   (if cc-hit
                       (set-cdr! cc-hit #t)
                       (begin
                         (set! cc-hit (cons (car (attribute-definition-circularity-definition att-def)) #t))
                         (hashtable-set! (attribute-instance-cycle-cache att) args cc-hit))))
                 (lambda ()
                   (set! result (apply (attribute-definition-equation att-def) n args)) ; Evaluate the attribute and...
                   (update-cycle-cache)) ; ...update its cycle-cache.
                 (lambda ()
                   ; Mark that the evaluation of the attribute is finished and...
                   (set-cdr! cc-hit #f)
                   ; ...pop the attribute from the attribute evaluation stack.
                   (evaluator-state-att-eval-stack-set! evaluator-state (cdr (evaluator-state-att-eval-stack evaluator-state))))))
               
               (entered? ; EVALUATION-CASE (4): Non-circular attribute already in evaluation.
                ; Maintaine attribute dependencies, i.e., the other attribute throughout whose evaluation
                ; this attribute is evaluated must depend on this one. Then,...
                (add-dependency:att->att att)
                ; ...thrown an exception because we encountered an unexpected dependency cycle.
                (throw-exception "AG evaluator exception; Unexpected " name " cycle."))
               
               (else ; EVALUATION-CASE (5): Non-circular attribute not in evaluation.
                (dynamic-wind
                 (lambda ()
                   ; Maintaine attribute dependencies, i.e., iff this attribute is evaluated throughout the evaluation
                   ; of another attribute, the other attribute depends on this one and...
                   (add-dependency:att->att att)
                   ; ...this attribute must depend on any other attributes that will be evaluated through its own evaluation. Further,..
                   (evaluator-state-att-eval-stack-set! evaluator-state (cons att (evaluator-state-att-eval-stack evaluator-state)))
                   ; ...mark, that the attribute is in evaluation, i.e.,...
                   (set! cc-hit (cons racr-nil #t)) ; ...construct an appropriate cycle-cache entry and...
                   (hashtable-set! (attribute-instance-cycle-cache att) args cc-hit)) ; ...add it to the attribute's cycle-cache.
                 (lambda ()
                   (set! result (apply (attribute-definition-equation att-def) n args)) ; Evaluate the attribute and...
                   (if (attribute-definition-cached? att-def) ; ...if caching is enabled...
                       (hashtable-set! (attribute-instance-value-cache att) args result))) ; ...cache its value.
                 (lambda ()
                   ; Mark that the attribute's evaluation finished, i.e., clear its cycle-cache. Finally,...
                   (hashtable-clear! (attribute-instance-cycle-cache att))
                   ; ...pop the attribute from the attribute evaluation stack.
                   (evaluator-state-att-eval-stack-set! evaluator-state (cdr (evaluator-state-att-eval-stack evaluator-state)))))))
             result))))) ; Return the computed value.
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                   Abstract Syntax Tree Access Interface                                                        ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; Given a node n, return its type. If the node is a list node, an exception is thrown. Note, that the given node never can be
 ; a terminal node, since terminal nodes themselves cannot be accessed (cf. ast-child), but only their value.
 ; DEPENDENCIES:
 ;  - Existence of a node of n's type
 (define ast-node-type
   (lambda (n)
     (if (ast-list-node? n) ; Remember: (node-terminal? n) is not possible
         (throw-exception "Cannot access type; List nodes have no type."))
     (add-dependency:att->node-type n)
     (vector-ref (car (ast-rule-production (node-ast-rule n))) 0)))
 
 ; Given a node n, return whether it represents a list of children, i.e., is a list node, or not.
 ; DEPENDENCIES:
 ;  - n is list node: Existence of a node which is a list node
 ;  - n is not list node: Existence of a node which is not a list node
 (define ast-list-node?
   (lambda (n)
     (let ((result (eq? (node-ast-rule n) 'list-node)))
       (add-dependency:att->node-list? n)
       result)))
 
 ; Given at least one AST node and another node or symbol representing an AST rule, return if the first argument is a
 ; sub-type of the second. The considered sub-type relationship is reflexive, i.e., every type is a sub-type of itself.
 ; An exception is thrown, iff non of the arguments is an AST node or any of the arguments is a list or terminal node!
 ; DEPENDENCIES:
 ;  - Iff the first argument a1 is a node: Existence of a node of a1's sub-type
 ;  - Iff the second argument a2 is a node: Existence of a node of a2's super-type
 (define ast-subtype?
   (lambda (a1 a2)
     (if (or
          (and (node? a1) (or (ast-list-node? a1) (node-terminal? a1)))
          (and (node? a2) (or (ast-list-node? a2) (node-terminal? a2))))
         (throw-exception "List nodes and terminals cannot be tested for sub-typing."))
     (if (and (not (node? a1)) (not (node? a2)))
         (throw-exception "Wrong argument types; At least one argument must be an AST node."))
     ((lambda (t1/t2)
        (and
         (car t1/t2)
         (cdr t1/t2)
         (or
          (eq? (car t1/t2) (cdr t1/t2))
          (find
           (lambda (subtype)
             (eq? (car t1/t2) subtype))
           (ast-rule-subtypes (cdr t1/t2))))))
      (if (symbol? a1)
          (let ((t2 (node-ast-rule a2)))
            (add-dependency:att->node-super-type a2)
            (cons
             (hashtable-ref (racr-specification-rules (ast-rule-specification t2)) a1 #f)
             t2))
          (if (symbol? a2)
              (let ((t1 (node-ast-rule a1)))
                (add-dependency:att->node-sub-type a1)
                (cons
                 t1
                 (hashtable-ref (racr-specification-rules (ast-rule-specification t1)) a2 #f)))
              (begin
                (add-dependency:att->node-sub-type a1)
                (add-dependency:att->node-super-type a2)
                (cons (node-ast-rule a1) (node-ast-rule a2))))))))
 
 ; Given a node, return its parent if it has any, otherwise thrown an exception.
 ; DEPENDENCIES:
 ;  - Existence of a parent
 (define ast-parent
   (lambda (n)
     (let ((parent (node-parent n)))
       (if parent
           (begin
             (add-dependency:att->node parent)
             parent)
           (throw-exception "Cannot access parent of roots.")))))
 
 ; Given a node, return its i'th child; If the node has no i'th child or is a terminal throw an exception.
 ; DEPENDENCIES:
 ;  - Existence of the respective child
 (define ast-child
   (lambda (i n)
     (if (node-terminal? n)
         (throw-exception "Cannot access child; Terminals have no children."))
     (cond
       ((symbol? i)
        (if (ast-list-node? n)
            (throw-exception "Cannot access list element by context-name; List-nodes define no context-names."))
        (let loop ((contexts (cdr (ast-rule-production (node-ast-rule n))))
                   (children (node-children n)))
          (if (null? contexts)
              (throw-exception "Cannot access non-existent " i " child.")
              (if (eq? (vector-ref (car contexts) 3) i)
                  (let ((child (car children)))
                    (add-dependency:att->node child)
                    (if (node-terminal? child)
                        (node-children child)
                        child))
                  (loop (cdr contexts) (cdr children))))))
       (else
        (if (and (>= i 1) (>= (length (node-children n)) i))
            (let ((child (list-ref (node-children n) (- i 1))))
              (add-dependency:att->node child)
              (if (node-terminal? child)
                  (node-children child)
                  child))
            (throw-exception "Cannot access non existent " i "'th child."))))))
 
 ; Given a node C, child of another node P, return P's i'th child S (thus, S is a sibling of C or C)
 ; DEPENDENCIES:
 ;  - Existence of a parent and its i'th child.
 (define ast-sibling
   (lambda (i n)
     (ast-child i (ast-parent n))))
 
 ; Given a node, return its position within its parent's list of children.
 ; DEPENDENCIES:
 ;  - Existence of the child
 (define ast-child-index
   (lambda (n)
     (add-dependency:att->node n)
     (let loop ((children (node-children (node-parent n)))
                (pos 0))
       (if (eq? (car children) n)
           (+ pos 1)
           (loop (cdr children) (+ pos 1))))))
 
 ; Given a node n, return its number of children. If the node is a terminal an exception is thrown.
 ; DEPENDENCIES:
 ;  - Existence of a node with n's number of children.
 (define ast-num-children
   (lambda (n)
     (if (node-terminal? n)
         (throw-exception "Cannot access number of children; Terminals have no children."))
     (add-dependency:att->node-num-children n)
     (length (node-children n))))
 
 ; Given a node and arbitrary many child-intervals, return its children (within the given intervals) as ordinary Scheme list.
 ; The returned list is a copy; Any changes on it (e.g., using set-cdr! or set-car!) do not change the AST!
 ; DEPENDENCIES: See ast-for-each-child
 (define-syntax ast-children
   (lambda (x)
     (syntax-case x ()
       ((_ n b ...)
        #'(reverse
           (let ((result (list)))
             (ast-for-each-child
              (lambda (i child)
                (set! result (cons child result)))
              n
              b ...)
             result))))))
 
 ; Given a function f, a node n and arbitrary many child-intervals b1,b2,...,bm (each a pair consisting of a
 ; lower bound lb and an upper bound ub), apply for each child-interval bi = (lb ub) the function f to each child c
 ; with index i with lb <= i <= ub, taking into account the order of child-intervals and children. Thereby, f must
 ; be of arity two; Each time f is called, its arguments are an index i and the respective i'th child of n.
 ; If no child-interval is given, the given function is applied to each child once. A child-interval with unbounded
 ; upper bound (ub = '*) means "apply the given function to every child with index >= the interval's lower bound".
 ; DEPENDENCIES:
 ;  - For every visisted child: Existence of the respective child
 ;  - Iff no interval or an interval with unbounded upper bound is given AND the visit is not aborted by
 ;    the application of a continuation: Existence of a node with n's number of children
 (define-syntax ast-for-each-child
   (lambda (x)
     (syntax-case x ()
       ((_ f n b)
        #'(let* ((f* f)
                 (n* n)
                 (b* b)
                 (ub (cdr b*)))
            (if (eq? ub '*)
                (let ((pos (car b*))
                      (ub (length (node-children n*))))
                  (dynamic-wind
                   (lambda () #f)
                   (lambda ()
                     (let loop ()
                       (if (<= pos ub)
                           (begin
                             (f* pos (ast-child pos n*))
                             (set! pos (+ pos 1))
                             (loop)))))
                   (lambda ()
                     (if (> pos ub)
                         (ast-num-children n*))))) ; BEWARE: Access to number of children ensures proper dependency tracking!
                (let loop ((pos (car b*)))
                  (if (<= pos ub)
                      (begin
                        (f* pos (ast-child pos n*))
                        (loop (+ pos 1))))))))
       ((_ f n)
        #'(ast-for-each-child f n (cons 1 '*)))
       ((_ f n b ...)
        #'(let ((f* f)
                (n* n))
            (ast-for-each-child f* n* b) ...)))))
 
 ; Find the first child within an AST node's children which satisfies a given filter function. The filter function must accept two
 ; parameters - a child index and the actual child. Additionally, arbitrary many child-intervals can be given. Iff no child
 ; satisfying the filter function exists, #f is retuned. Otherwise the founded child.
 ; DEPENDENCIES: See ast-for-each-child
 (define-syntax ast-find-child
   (lambda (x)
     (syntax-case x ()
       ((_ f n b ...)
        #'(let ((f* f))
            (call/cc
             (lambda (c)
               (ast-for-each-child
                (lambda (i child)
                  (if (f* i child)
                      (c child)))
                n
                b ...)
               #f)))))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                Abstract Syntax Tree Construction Interface                                                     ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; Function for the construction of proper attributed, rewriteable derivation fragments. Given a RACR specification, the name of a non-terminal
 ; to construct (i.e., an AST rule to apply) and a list of children, the function constructs and returns an AST fragment, whose root node's type
 ; and children are the given ones. Thereby, it is checked, that (1) the given children fit into the AST fragment to construct, (2) enough and
 ; not to many children are given and (3) inherited attribute instances introduced for children are shadowed by equally named synthesized attributes
 ; the children already have. In case of any violation an exception is thrown. Additionally, an exception is thrown, if attributes of any of the
 ; given children are in evaluation.
 (define create-ast
   (lambda (spec rule children)
     ;;; Ensure, that the RACR system is completely specified:
     (if (< (racr-specification-specification-phase spec) 3)
         (throw-exception "Cannot construct " rule " fragment; The RACR specification still must be compiled."))
     
     (let ((ast-rule* (hashtable-ref (racr-specification-rules spec) rule #f)))
       ;;; Ensure, that the given AST rule is defined:
       (if (not ast-rule*)
           (throw-exception "Cannot construct " rule " fragment; Unknown non-terminal/rule."))
       
       ;;; Ensure, that the expected number of children are given:
       (if (not (= (length children) (- (length (ast-rule-production ast-rule*)) 1)))
           (throw-exception
            "Cannot construct "
            rule
            " fragment; "
            (length children)
            " children given, but "
            (- (length (ast-rule-production ast-rule*)) 1)
            " children expected."))
       
       ;;; Construct the fragment, i.e., (1) the AST part consisting of the root and the given children and (2) the root's
       ;;; synthesized attribute instances and the childrens' inherited ones.
       (let (;;; For (1) --- the construction of the fragment's AST part --- first construct the fragment's root. Then...
             (root
              (make-node
               ast-rule*
               #f
               (list))))
         (node-children-set! ; ...ensure, that the given children fit and add them to the fragment to construct. Therefore,...
          root
          (let loop ((pos 1) ; ...investigate every...
                     (symbols (cdr (ast-rule-production ast-rule*))) ; ...expected and...
                     (children children)) ; ...given child....
            (if (null? symbols) ; ...If no further child is expected,...
                (list) ; ...we are done, otherwise...
                (let ((symb* (car symbols))
                      (child (car children)))
                  (if (vector-ref symb* 1) ; ...check if the next expected child is a non-terminal....
                      (let ((ensure-child-fits ; ...If we expect a non-terminal we need a function which ensures, that...
                             (lambda (child)
                               ; ...its type is the one of the expected non-terminal or a sub-type....
                               (if (not (memq (node-ast-rule child) (cons (vector-ref symb* 1) (ast-rule-subtypes (vector-ref symb* 1)))))
                                   (throw-exception
                                    "Cannot construct "
                                    rule
                                    " fragment; Expected a "
                                    (vector-ref symb* 0)
                                    " node as "
                                    pos
                                    "'th child, not a "
                                    (ast-node-type child)
                                    ".")))))
                        (if (not (node? child)) ; ...Then, check that the given child is an AST node,...
                            (throw-exception
                             "Cannot construct "
                             rule
                             " fragment; Expected a "
                             (vector-ref symb* 0)
                             " node as "
                             pos
                             "'th child, not a terminal."))
                        (if (node-parent child) ; ...does not already belong to another AST and...
                            (throw-exception
                             "Cannot construct "
                             rule
                             " fragment. The given "
                             pos
                             "'th child already is part of another AST fragment."))
                        (if (evaluator-state-in-evaluation? (node-evaluator-state child)) ; ...non of its attributes are in evaluation....
                            (throw-exception "Cannot construct " rule " fragment; There are attributes in evaluation."))
                        (if (vector-ref symb* 2) ; ...Now, check if we expect a list of non-terminals...
                            (if (ast-list-node? child) ; ...If we expect a list, ensure the given child is a list-node and...
                                (for-each ensure-child-fits (node-children child)) ; ...all its elements fit....
                                (throw-exception
                                 "Cannot construct "
                                 rule
                                 " fragment; Expected a list-node as "
                                 pos
                                 "'th child, not a "
                                 (if (node? child)
                                     (string-append "single [" (symbol->string (ast-node-type child)) "] node")
                                     "terminal")
                                 "."))
                            (ensure-child-fits child)) ; ...If we expect a single non-terminal child, just ensure that the child fits....
                        (node-parent-set! child root) ; ...Finally, set the root as the child's parent,...
                        (cons
                         child ; ...add the child to the root's children and...
                         (loop (+ pos 1) (cdr symbols) (cdr children)))) ; ...process the next expected child.
                      (cons ; If we expect a terminal,...
                       (make-node ; ...add a terminal node encapsulating the given value to the root's children and...
                        'terminal
                        root
                        child)
                       (loop (+ pos 1) (cdr symbols) (cdr children)))))))) ; ...process the next expected child.
         (distribute-evaluator-state (make-evaluator-state) root) ; ...When all children are processed, distribute the new fragment's evaluator state.
         
         ;;; The AST part of the fragment is properly constructed so we can proceed with (2) --- the construction
         ;;; of the fragment's attribute instances. Therefore,...
         (attach-synthesized-attributes root) ; ...initialize the root's synthesized and...
         (for-each ; ...each child's inherited attributes.
          attach-inherited-attributes
          (node-children root))
         
         root)))) ; Finally, return the newly constructed fragment.
 
 ; Given a list l of non-terminal nodes that are not list-nodes construct a list-node whose elements are the elements of l.
 ; An exception is thrown, iff an element of l is not an AST node, is a list-node, is a terminal node, already belongs to
 ; another AST, has attributes in evaluation or at least two elements of l are instances of different RACR specifications.
 (define create-ast-list
   (lambda (children)
     (let loop ((children children) ; For every child, ensure, that...
                (pos 1))
       (if (not (null? children))
           (begin
             ; ...the child is a non-terminal, non-list node,...
             (if (or (not (node? (car children))) (ast-list-node? (car children)) (node-terminal? (car children)))
                 (throw-exception "Cannot construct list-node; The given " pos "'th child is not a non-terminal, non-list node."))
             ; ...is not already part of another AST and...
             (if (node-parent (car children))
                 (throw-exception "Cannot construct list-node; The given " pos "'th child already is part of another AST."))
             (if (evaluator-state-in-evaluation? (node-evaluator-state (car children))) ; ...non of its attributes are in evaluation. Further,...
                 (throw-exception "Cannot construct list-node; The given " pos "'th child has attributes in evaluation."))
             (loop (cdr children) (+ pos 1)))))
     (if (not (null? children)) ; ...ensure, that all children are instances of the same RACR specification....
         (let ((spec (ast-rule-specification (node-ast-rule (car children)))))
           (for-each
            (lambda (child)
              (if (not (eq? (ast-rule-specification (node-ast-rule child)) spec))
                  (throw-exception "Cannot construct list-node; The given children are instances of different RACR specifications.")))
            children)))
     (let ((list-node ; ...Finally, construct the list-node,...
            (make-node
             'list-node
             #f
             children)))
       (for-each ; ...set it as parent for every of its elements,...
        (lambda (child)
          (node-parent-set! child list-node))
        children)
       (distribute-evaluator-state (make-evaluator-state) list-node) ; ...construct and distribute its evaluator state and...
       list-node))) ; ...return it.
 
 ; INTERNAL FUNCTION: Given an AST node initialize its synthesized attribute instances (i.e., add the synthesized attributes
 ; defined for it). Respective attributes are only added, iff the node does not already have equally named attributes.
 (define attach-synthesized-attributes
   (lambda (n)
     (if (and (not (node-terminal? n)) (not (ast-list-node? n)))
         (for-each
          (lambda (att-def)
            (if (not (find-attribute (attribute-definition-name att-def) n))
                (node-attributes-set! n (cons (make-attribute-instance att-def n) (node-attributes n)))))
          (filter
           attribute-definition-synthesized?
           (ast-rule-attributes (node-ast-rule n)))))))
 
 ; INTERNAL FUNCTION: Given an AST node initialize its inherited attribute instances considering its context, whereby
 ; attributes are only added, iff the node does not already have equally named attributes. Iff the given node is a
 ; list-node, the respective inherited attributes are added to each of its elements.
 (define attach-inherited-attributes
   (lambda (n)
     (cond
       ((ast-list-node? n)
        (for-each
         attach-inherited-attributes
         (node-children n)))
       ((node-non-terminal? n)
        (let ((n* (if (ast-list-node? (node-parent n))
                      (node-parent n)
                      n)))
          (for-each
           (lambda (att-def)
             (if (not (find-attribute (attribute-definition-name att-def) n))
                 (node-attributes-set! n (cons (make-attribute-instance att-def n) (node-attributes n)))))
           (filter
            (lambda (att-def)
              (and
               (attribute-definition-inherited? att-def)
               (= (cdr (attribute-definition-context att-def)) (ast-child-index n*))))
            (ast-rule-attributes (node-ast-rule (node-parent n*))))))))))
 
 ; INTERNAL FUNCTION: Given an AST node delete its inherited attribute instances. Iff the given node is a list node,
 ; the inherited attributes of its elements are deleted.
 (define detach-inherited-attributes
   (lambda (n)
     (cond
       ((ast-list-node? n)
        (for-each
         detach-inherited-attributes
         (node-children n)))
       ((node-non-terminal? n)
        (node-attributes-set!
         n
         (remp
          (lambda (att)
            (let ((remove? (attribute-definition-inherited? (attribute-instance-definition att))))
              (if remove?
                  (begin
                    (flush-attribute-cache att)
                    (attribute-instance-context-set! att racr-nil)))
              remove?))
          (node-attributes n)))))))
 
 ; INTERNAL FUNCTION: Given an evaluator state and an AST fragment, change the fragment's evaluator state to the given one.
 (define distribute-evaluator-state
   (lambda (evaluator-state n)
     (node-evaluator-state-set! n evaluator-state)
     (if (not (node-terminal? n))
         (for-each
          (lambda (n)
            (distribute-evaluator-state evaluator-state n))
          (node-children n)))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                             Rewrite Interface                                                                  ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; Given an AST node to replace (old fragment) and its replacement (new fragment) replace the old fragment by the new one.
 ; Thereby, any influenced attributes' caches are flushed and dependencies are maintained. An exception is thrown, iff the
 ; new fragment doesn't fit, the old fragment is an AST root (i.e., a start symbol node), any attributes of either fragment
 ; are in evaluation or the new fragment already is part of another AST.
 (define rewrite-node
   (lambda (old-fragment new-fragment)
     (let* (; Support variable constraining the type of the new fragment depending on the type of replacement:
            ; In case of list-node replacement: The type the new fragment's elements must be
            ; In case of non-list non-terminal (ordinary non-terminal) replacement: The type the new fragment must be
            ; In case of terminal replacement: #f
            (expected-type
             (cond
               ((node-terminal? old-fragment)
                #f)
               ((ast-list-node? (node-parent old-fragment))
                (vector-ref
                 (list-ref
                  (ast-rule-production (node-ast-rule (node-parent (node-parent old-fragment))))
                  (ast-child-index (node-parent old-fragment)))
                 1))
               (else
                (vector-ref
                 (list-ref
                  (ast-rule-production (node-ast-rule (node-parent old-fragment)))
                  (ast-child-index old-fragment))
                 1))))
            (valid-types
             (and expected-type (cons expected-type (ast-rule-subtypes expected-type))))
            ; Support function to throw replacement exceptions incorporating a given error message:
            (error
             (lambda (cause)
               (throw-exception
                "Cannot perform replacement [old: "
                (symbol->string
                 (if (or (ast-list-node? old-fragment) (node-terminal? old-fragment))
                     (node-ast-rule old-fragment)
                     (vector-ref (car (ast-rule-production (node-ast-rule old-fragment))) 0)))
                " | new: "
                (symbol->string
                 (if (or (ast-list-node? new-fragment) (node-terminal? new-fragment))
                     (node-ast-rule new-fragment)
                     (vector-ref (car (ast-rule-production (node-ast-rule new-fragment))) 0)))
                " | expected: "
                (if (ast-list-node? old-fragment)
                    "list of "
                    "")
                (if expected-type
                    (symbol->string (vector-ref (car (ast-rule-production expected-type)) 0))
                    "terminal")
                "]; "
                cause)))
            ; Support function used to perform VALID replacements. To perform a replacement,...
            (insert-into-ast
             (lambda ()
               (let loop ((n old-fragment)) ; ...for every node within the AST the old fragment spans,...
                 (for-each ; ...flush the caches of all attributes influenced by the node and...
                  (lambda (influence)
                    (flush-attribute-cache (car influence)))
                  (node-attribute-influences n))
                 (for-each ; ...all its attributes. Then,...
                  flush-attribute-cache
                  (node-attributes n))
                 (if (node-non-terminal? n)
                     (for-each
                      loop
                      (node-children n))))
               (detach-inherited-attributes old-fragment) ; ...delete the old fragment's inherited attributes,...
               (let loop ((children (node-children (node-parent old-fragment)))) ; ...insert the new fragment at the right place and...
                 (if (eq? (car children) old-fragment)
                     (set-car! children new-fragment)
                     (loop (cdr children))))
               (node-parent-set! new-fragment (node-parent old-fragment))
               (node-parent-set! old-fragment #f) ; ...detach the old fragment. Further,...
               (distribute-evaluator-state (node-evaluator-state old-fragment) new-fragment) ; ...initialize the new fragment's evaluator state and...
               (distribute-evaluator-state #f old-fragment) ; ...reset the old fragment's one. Finally,...
               (attach-inherited-attributes new-fragment)))) ; ...initialize any inherited attributes defined for the new fragment in its new context.
       ; Before performing the actual replacement, check that neither, attributes of the old nor the new fragment are in evaluation,
       ; no start symbol replacement is given and the new fragment is not part of another AST (i.e., is freely available). Further,...
       (if (and
            (node-evaluator-state old-fragment)
            (evaluator-state-in-evaluation? (node-evaluator-state old-fragment)))
           (error "There are attributes in evaluation."))
       (if (and
            expected-type
            (eq?
             (vector-ref (car (ast-rule-production expected-type)) 0)
             (racr-specification-start-symbol (ast-rule-specification (node-ast-rule old-fragment)))))
           (error "Replacement of start symbol fragments not permitted."))
       (if (node-parent new-fragment)
           (error "The given replacement already is part of another AST."))
       ; ...depending, whether (1) a terminal, (2) a list-node or (3) ordinary non-terminal replacement is given we must ensure that the
       ; new fragment is a terminal, list-node or ordinary non-terminal respectively and, in case of a list-node, its children fit or,
       ; in case of an ordinary non-terminal, the new fragment itself fits into its new context:
       (cond
         ; (1) Terminal replacement:
         ((node-terminal? old-fragment)
          (if (not (node-terminal? new-fragment))
              (error "The given replacement does not fit."))
          (insert-into-ast)) ; Perform the actual replacement.
         ; (2) List-node replacement:
         ((ast-list-node? old-fragment)
          (if (not (ast-list-node? new-fragment))
              (error "The given replacement does not fit."))
          (for-each
           (lambda (child)
             (if (not (memq (node-ast-rule child) valid-types))
                 (error
                  (string-append
                   "The given list-node contains a non-fitting ["
                   (symbol->string (ast-node-type child))
                   "] child."))))
           (node-children new-fragment))
          (insert-into-ast)) ; Perform the actual replacement.
         ; (3) Ordinary non-terminal replacement:
         (else
          (if (or
               (node-terminal? new-fragment)
               (ast-list-node? new-fragment)
               (not (memq (node-ast-rule new-fragment) valid-types)))
              (error "The given replacement does not fit."))
          (insert-into-ast)))))) ; Perform the actual replacement.
 
 ; Given a node n, a child-index i and an arbitrary value, change the value of n's i'th child, which must be a terminal, to
 ; the given one. Thereby, any influenced attributes' caches are flushed and dependencies are maintained. An exception is
 ; thrown, iff n has no i'th child, n's i'th child is no terminal or any attributes of the AST n is part of are in evaluation.
 (define rewrite-terminal
   (lambda (i n new-value)
     (if (not (equal? (ast-child i n) new-value))
         (rewrite-node
          (list-ref (node-children n) (- i 1))
          (make-node 'terminal #f new-value)))))
 
 ; Given a list-node l, a child-index i and an AST node e, insert e as i'th element into l. Thereby, any influenced attributes'
 ; caches are flushed and dependencies are maintained. An exception is thrown, iff l is no list-node, e does not fit w.r.t.
 ; l's context, l has not enough elements, such that no i'th position exists, any attributes of either l or e are in evaluation
 ; or e already is part of another AST.
 (define rewrite-insert
   (lambda (l i e)
     ; Before inserting the element, ensure, that...
     (if (not (ast-list-node? l)) ; ...indeed a list-node is given as context,...
         (throw-exception "Cannot insert list element; The given context is no list-node."))
     (letrec* (; Support variable constraining the type of the element to insert:
               (expected-type
                (vector-ref
                 (list-ref
                  (ast-rule-production (node-ast-rule (node-parent l)))
                  (ast-child-index l))
                 1))
               (valid-types (cons expected-type (ast-rule-subtypes expected-type)))
               ; Support function to throw insertion exceptions incorporating a given error message:
               (error
                (lambda (cause)
                  (throw-exception
                   "Cannot insert list element [index: "
                   (number->string i)
                   " | new: "
                   (symbol->string
                    (if (or (ast-list-node? e) (node-terminal? e))
                        (node-ast-rule e)
                        (vector-ref (car (ast-rule-production (node-ast-rule e))) 0)))
                   " | expected: "
                   (symbol->string (vector-ref (car (ast-rule-production expected-type)) 0))
                   "]; "
                   cause)))
               ; Support function to insert an element e at position i into a list l:
               (insert
                (lambda (l i e)
                  (if (= i 0)
                      (cons e l)
                      (cons (car l) (insert (cdr l) (- i 1) e))))))
              (if (evaluator-state-in-evaluation? (node-evaluator-state l)) ; ...no attributes are in evaluation,...
                  (error "There are attributes in evaluation."))
              (if (node-parent e) ; ...the new element is not part of another AST (i.e., is freely available),...
                  (error "The given element already is part of another AST."))
              (if (or (< i 1) (> i (+ (ast-num-children l) 1))) ; ...the list has enough elements and...
                  (error "The given index is out of range."))
              (if (not (memq (node-ast-rule e) valid-types)) ; ...the element can be a child of the list-node....
                  (error "The new element does not fit."))
              ; ...When all rewrite constraints are satisfied...
              (for-each ; ...flush the caches of all attributes influenced by the list-node's number of children. Further,...
               (lambda (influence)
                 (if (vector-ref (cdr influence) 1)
                     (flush-attribute-cache (car influence))))
               (node-attribute-influences l))
              (for-each ; ...for every node within the ASTs of the successor elements following the insertion position,...
               (lambda (n)
                 (let loop ((n n))
                   (for-each ; ...flush the caches of all attributes influenced by the node and...
                    (lambda (influence)
                      (flush-attribute-cache (car influence)))
                    (node-attribute-influences n))
                   (for-each ; ...all its attributes. Afterwards,...
                    flush-attribute-cache
                    (node-attributes n))
                   (if (node-non-terminal? n)
                       (for-each
                        loop
                        (node-children n)))))
               (list-tail (node-children l) (- i 1)))
              (node-children-set! l (insert (node-children l) (- i 1) e)) ; ...insert the new element,...
              (node-parent-set! e l)
              (distribute-evaluator-state (node-evaluator-state l) e) ; ...initialize its evaluator state and...
              (attach-inherited-attributes e)))) ; ...any inherited attributes defined for its new context.
 
 ; Given a node, which is element of a list-node (i.e., its parent node is a list-node), delete it within the list. Thereby,
 ; any influenced attributes' caches are flushed and dependencies are maintained. An exception is thrown, iff the given node
 ; is no list-node element or any attributes of the AST it is part of are in evaluation.
 (define rewrite-delete
   (lambda (n)
     ; Before deleting the element ensure, that...
     (if (evaluator-state-in-evaluation? (node-evaluator-state n)) ; ...no attributes are in evaluation and...
         (throw-exception "Cannot delete list element; There are attributes in evaluation."))
     (if (not (ast-list-node? (node-parent n))) ; ...the given node is a list-node element....
         (throw-exception "Cannot delete list element; The given node is not element of a list."))
     ; ...When all rewrite constraints are satisfied,...
     (for-each ; ...flush the caches of all attributes influenced by the number of children of the list-node the element is part of. Further,...
      (lambda (influence)
        (if (vector-ref (cdr influence) 1)
            (flush-attribute-cache (car influence))))
      (node-attribute-influences (node-parent n)))
     (for-each ; ...for every node within the ASTs the element and its successor elements span,...
      (lambda (n)
        (let loop ((n n))
          (for-each ; ...flush the caches of all attributes influenced by the node and...
           (lambda (influence)
             (flush-attribute-cache (car influence)))
           (node-attribute-influences n))
          (for-each ; ...all its attributes. Afterwards,...
           flush-attribute-cache
           (node-attributes n))
          (if (node-non-terminal? n)
              (for-each
               loop
               (node-children n)))))
      (list-tail (node-children (node-parent n)) (- (ast-child-index n) 1)))
     (detach-inherited-attributes n) ; ...delete the element's inherited attributes,...
     (node-children-set! (node-parent n) (remq n (node-children (node-parent n)))) ; ...remove it from the list and...
     (node-parent-set! n #f)
     (distribute-evaluator-state #f n))) ; ...reset its evaluator state.
 
 ; Given a list-node l and another node e add e to l's list of children (i.e., e becomes an element of l). Thereby, any influenced
 ; attributes' caches are flushed and dependencies are maintained. An exception is thrown, iff l is not a list-node, e does not fit
 ; w.r.t. l's context, any attributes of either l or e are in evaluation or e already is part of another AST.
 (define rewrite-add
   (lambda (l e)
     ; Before adding the element, ensure, that...
     (if (evaluator-state-in-evaluation? (node-evaluator-state l)) ; ...no attributes are in evaluation,...
         (throw-exception "Cannot add list element; There are attributes in evaluation."))
     (if (not (ast-list-node? l)) ; ...indeed a list-node is given as context,...
         (throw-exception "Cannot add list element; The given context is no list-node."))
     (if (node-parent e) ; ...the new element is not part of another AST (i.e., is freely available) and...
         (throw-exception "Cannot add list element; The given replacement already is part of another AST."))
     (let* ((expected-type
             (vector-ref
              (list-ref
               (ast-rule-production (node-ast-rule (node-parent l)))
               (ast-child-index l))
              1))
            (valid-types (cons expected-type (ast-rule-subtypes expected-type))))
       (if (not (memq (node-ast-rule e) valid-types)) ; ...can be a child of the list-node....
           (throw-exception "Cannot add list element; The new element does not fit.")))
     ; ...When all rewrite constraints are satisfied...
     (for-each ; ...flush the caches of all attributes influenced by the list-node's number of children,...
      (lambda (influence)
        (if (vector-ref (cdr influence) 1)
            (flush-attribute-cache (car influence))))
      (node-attribute-influences l))
     (node-children-set! l (append (node-children l) (list e))) ; ...add the new element,...
     (node-parent-set! e l)
     (distribute-evaluator-state (node-evaluator-state l) e) ; ...initialize its evaluator state and...
     (attach-inherited-attributes e))) ; ...any inherited attributes defined for its new context.
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                        Dependency Tracking Support                                                             ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; INTERNAL FUNCTION: Given an attribute, flush its and its depending attributes' caches and dependencies.
 (define flush-attribute-cache
   (lambda (att)
     (let ((influenced-atts (attribute-instance-attribute-influences att))) ; First, save all attributes influenced by the attribute,...
       (attribute-instance-attribute-influences-set! att (list)) ; ...remove the respective influence edges and...
       (hashtable-clear! (attribute-instance-value-cache att)) ; ...clear the attribute's value cache. Then,...
       (for-each ; ...for every attribute I the attribute depends on,...
        (lambda (influencing-att)
          (attribute-instance-attribute-influences-set!
           influencing-att
           (remq att (attribute-instance-attribute-influences influencing-att)))) ; ...remove the influence edge from I to the attribute and...
        (attribute-instance-attribute-dependencies att))
       (attribute-instance-attribute-dependencies-set! att (list)) ;...the attribute's dependency edges to such I. Then,...
       (for-each ; ...for every node N the attribute depends on...
        (lambda (node-influence)
          (node-attribute-influences-set!
           (car node-influence)
           (remp ; ...remove the influence edge from N to the attribute and...
            (lambda (attribute-influence)
              (eq? (car attribute-influence) att))
            (node-attribute-influences (car node-influence)))))
        (attribute-instance-node-dependencies att))
       (attribute-instance-node-dependencies-set! att (list)) ; ...the attribute's dependency edges to such N. Finally,...
       (for-each ; ...for every attribute D the attribute originally influenced,...
        (lambda (dependent-att)
          (flush-attribute-cache dependent-att)) ; ...flush D.
        influenced-atts))))
 
 ; INTERNAL FUNCTION: See "add-dependency:att->node-characteristic".
 (define add-dependency:att->node
   (lambda (influencing-node)
     (add-dependency:att->node-characteristic influencing-node 0)))
 
 ; INTERNAL FUNCTION: See "add-dependency:att->node-characteristic".
 (define add-dependency:att->node-num-children
   (lambda (influencing-node)
     (add-dependency:att->node-characteristic influencing-node 1)))
 
 ; INTERNAL FUNCTION: See "add-dependency:att->node-characteristic".
 (define add-dependency:att->node-type
   (lambda (influencing-node)
     (add-dependency:att->node-characteristic influencing-node 2)))
 
 ; INTERNAL FUNCTION: See "add-dependency:att->node-characteristic".
 (define add-dependency:att->node-super-type
   (lambda (influencing-node)
     (add-dependency:att->node-characteristic influencing-node 3)))
 
 ; INTERNAL FUNCTION: See "add-dependency:att->node-characteristic".
 (define add-dependency:att->node-sub-type
   (lambda (influencing-node)
     (add-dependency:att->node-characteristic influencing-node 4)))
 
 ; INTERNAL FUNCTION: See "add-dependency:att->node-characteristic".
 (define add-dependency:att->node-list?
   (lambda (influencing-node)
     (add-dependency:att->node-characteristic influencing-node 5)))
 
 ; INTERNAL FUNCTION: Given a node N and an influencing-characteristic C add an dependency-edge marked with C from
 ; the attribute currently in evaluation (considering the evaluator state of the AST N is part of) to N and an influence-edge
 ; vice versa. If no attribute is in evaluation no edges are added. The following six influencing-characteristics exist:
 ;  1) Dependency on the existence of the node (i.e., existence of a node at the same location)
 ;  2) Dependency on the node's number of children (i.e., existence of a node at the same location and with the same number of children)
 ;  3) Dependency on the node's type (i.e., existence of a node at the same location and with the same type)
 ;  4) Dependency on the node's super-type (i.e., existence of a node at the same location which is a super-type)
 ;  5) Dependency on the node's sub-type (i.e., existence of a node at the same location which is a sub-type)
 ;  6) Dependency on whether the node is a list-node or not (i.e., existence of a node at the same location which also is a/no list-node)
 (define add-dependency:att->node-characteristic
   (lambda (influencing-node influencing-characteristic)
     (let ((dependent-att (evaluator-state-in-evaluation? (node-evaluator-state influencing-node))))
       (if dependent-att
           (let ((dependency-vector
                  (let ((dc-hit (assq influencing-node (attribute-instance-node-dependencies dependent-att))))
                    (and dc-hit (cdr dc-hit)))))
             (if (not dependency-vector)
                 (begin
                   (set! dependency-vector (vector #f #f #f #f #f #f))
                   (attribute-instance-node-dependencies-set!
                    dependent-att
                    (cons
                     (cons influencing-node dependency-vector)
                     (attribute-instance-node-dependencies dependent-att)))
                   (node-attribute-influences-set!
                    influencing-node
                    (cons
                     (cons dependent-att dependency-vector)
                     (node-attribute-influences influencing-node)))))
             (vector-set! dependency-vector influencing-characteristic #t))))))
 
 ; INTERNAL FUNCTION: Given an attribute instance A, add an dependency-edge from A to the attribute currently in evaluation (considering
 ; the evaluator state of the AST A is part of) and an influence-edge vice-versa. If no attribute is in evaluation no edges are added.
 (define add-dependency:att->att
   (lambda (influencing-att)
     (let ((dependent-att (evaluator-state-in-evaluation? (node-evaluator-state (attribute-instance-context influencing-att)))))
       (if (and dependent-att (not (memq influencing-att (attribute-instance-attribute-dependencies dependent-att))))
           (begin
             (attribute-instance-attribute-dependencies-set!
              dependent-att
              (cons
               influencing-att
               (attribute-instance-attribute-dependencies dependent-att)))
             (attribute-instance-attribute-influences-set!
              influencing-att
              (cons
               dependent-att
               (attribute-instance-attribute-influences influencing-att)))))))))