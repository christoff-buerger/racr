; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (racr-meta core)
 (export
  ; Specification interface:
  (rename (make-racr-specification create-specification))
  ; Specification query interface:
  specification->phase
  specification->start-symbol
  specification->ast-rules
  specification->find-ast-rule
  ast-rule->symbolic-representation
  ast-rule->supertype?
  ast-rule->production
  symbol->name
  symbol->non-terminal?
  symbol->kleene?
  symbol->context-name
  symbol->attributes
  attribute->name
  attribute->circular?
  attribute->synthesized?
  attribute->inherited?
  attribute->cached?
  ; ASTs: Specification
  (rename (specify-ast-rule ast-rule))
  compile-ast-specifications
  ; ASTs: Construction
  create-ast
  create-ast-list
  create-ast-bud
  create-ast-mockup
  ; ASTs: Traversal
  ast-parent
  ast-child
  ast-sibling
  ast-children
  ast-for-each-child
  ast-find-child
  ast-find-child*
  ; ASTs: Node Information
  ast-node?
  ast-specification
  ast-has-parent?
  ast-child-index
  ast-has-child?
  ast-num-children
  ast-has-sibling?
  ast-node-type
  ast-node-rule
  ast-list-node?
  ast-bud-node?
  ast-subtype?
  ; Attribution: Specification
  specify-attribute
  (rename (specify-ag-rule ag-rule))
  compile-ag-specifications
  ; Attribution: Querying
  att-in-evaluation?
  att-value
  ; Rewriting: Primitive Rewrite Functions
  rewrite-terminal
  rewrite-refine
  rewrite-abstract
  rewrite-subtree
  rewrite-add
  rewrite-insert
  rewrite-delete
  ; Rewriting: Rewrite Strategies
  perform-rewrites
  ; Support
  with-specification
  ; RACR Exceptions:
  throw-exception
  racr-exception?
  
  ; TODO Delete following exports when properly integrated:
  (rename (make-racr-specification-2 create-specification-2))
  racr-specification-2-ast-scheme
  (rename (specify-ast-rule-2 ast-rule-2))
  specify-start-symbol-2
  specify-attribute-2
  compile-specification-2
  create-ast-2
  create-ast-list-2)
 (import (rnrs) (rnrs mutable-pairs))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Internal Data Structures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; Constructor for unique entities internally used by the RACR system
 (define-record-type racr-nil-record (sealed #t)(opaque #t))
 (define racr-nil (make-racr-nil-record)) ; Unique value indicating undefined RACR entities
 
 ; Record type representing RACR compiler specifications. A compiler specification consists of arbitrary
 ; many AST rule, attribute and rewrite specifications, all aggregated into a set of rules stored in a
 ; non-terminal-symbol -> ast-rule hashtable, an actual compiler specification phase and a distinguished
 ; start symbol. The specification phase is an internal flag indicating the RACR system the compiler's
 ; specification progress. Possible phases are:
 ; 1 : AST specification
 ; 2 : AG specification
 ; 3 : Rewrite specification
 ; 4 : Specification finished
 (define-record-type racr-specification
   (fields (mutable specification-phase) rules-table (mutable start-symbol))
   (opaque #t)(sealed #t)
   (protocol
    (lambda (new)
      (lambda ()
        (new 1 (make-eq-hashtable 50) racr-nil)))))
 
 ; INTERNAL FUNCTION: Given a RACR specification and a non-terminal, return the
 ; non-terminal's AST rule or #f if it is undefined.
 (define racr-specification-find-rule
   (lambda (spec non-terminal)
     (hashtable-ref (racr-specification-rules-table spec) non-terminal #f)))
 
 ; INTERNAL FUNCTION: Given a RACR specification return a list of its AST rules.
 (define racr-specification-rules-list
   (lambda (spec)
     (call-with-values
      (lambda ()
        (hashtable-entries (racr-specification-rules-table spec)))
      (lambda (key-vector value-vector)
        (vector->list value-vector)))))
 
 ; Record type for AST rules; An AST rule has a reference to the RACR specification it belongs to and consist
 ; of its symbolic encoding, a production (i.e., a list of production-symbols) and an optional supertype.
 (define-record-type ast-rule
   (fields specification as-symbol (mutable production) (mutable supertype?))
   (opaque #t)(sealed #t))
 
 ; INTERNAL FUNCTION: Given an AST rule find a certain child context by name. If the rule defines no such
 ; context, return #f, otherwise the production symbol defining the respective context.
 (define ast-rule-find-child-context
   (lambda (r context-name)
     (find
      (lambda (symbol)
        (eq? (symbol-context-name symbol) context-name))
      (cdr (ast-rule-production r)))))
 
 ; INTERNAL FUNCTION: Given two rules r1 and r2, return whether r1 is a subtype of r2 or not. The subtype
 ; relationship is reflexive, i.e., every type is a subtype of itself.
 ; BEWARE: Only works correct if supertypes are resolved, otherwise an exception can be thrown!
 (define ast-rule-subtype?
   (lambda (r1 r2)
     (and
      (eq? (ast-rule-specification r1) (ast-rule-specification r2))
      (let loop ((r1 r1))
        (cond
          ((eq? r1 r2) #t)
          ((ast-rule-supertype? r1) (loop (ast-rule-supertype? r1)))
          (else #f))))))
 
 ; INTERNAL FUNCTION: Given a rule, return a list containing all its subtypes except the rule itself.
 ; BEWARE: Only works correct if supertypes are resolved, otherwise an exception can be thrown!
 (define ast-rule-subtypes
   (lambda (rule1)
     (filter
      (lambda (rule2)
        (and (not (eq? rule2 rule1)) (ast-rule-subtype? rule2 rule1)))
      (racr-specification-rules-list (ast-rule-specification rule1)))))
 
 ; Record type for production symbols; A production symbol is part of a certain ast rule and has name,
 ; a flag indicating whether it is a non-terminal or not (later resolved to the actual AST rule representing
 ; the respective non-terminal), a flag indicating whether it represents a Kleene closure (i.e., is a list
 ; of certain type) or not, a context-name unambiguously referencing it within the production it is part of
 ; and a list of attributes defined for it.
 (define-record-type (symbol make-production-symbol production-symbol?)
   (fields name ast-rule (mutable non-terminal?) kleene? context-name (mutable attributes))
   (opaque #t)(sealed #t))
 
 ; Record type for attribute definitions. An attribute definition has a certain name, a definition context
 ; (i.e., a symbol of an AST rule), an equation and an optional circularity-definition used for fix-point
 ; computations. Further, attribute definitions specify whether the value of instances of the defined
 ; attribute are cached. Circularity-definitions are (bottom-value equivalence-function) pairs, whereby
 ; bottom-value is the value fix-point computations start with and equivalence-functions are used to decide
 ; whether a fix-point is reached or not (i.e., equivalence-functions are arbitrary functions of arity two
 ; computing whether two given arguments are equal or not).
 (define-record-type attribute-definition
   (fields name context equation circularity-definition cached?)
   (opaque #t)(sealed #t))
 
 ; INTERNAL FUNCTION: Given an attribute definition, check if instances can depend on
 ; themself (i.e., be circular) or not.
 (define attribute-definition-circular?
   (lambda (att)
     (if (attribute-definition-circularity-definition att) #t #f)))
 
 ; INTERNAL FUNCTION: Given an attribute definition, return whether it specifies
 ; a synthesized attribute or not.
 (define attribute-definition-synthesized?
   (lambda (att-def)
     (let ((symbol (attribute-definition-context att-def)))
       (eq? (car (ast-rule-production (symbol-ast-rule symbol))) symbol))))
 
 ; INTERNAL FUNCTION: Given an attribute definition, return whether it specifies
 ; an inherited attribute or not.
 (define attribute-definition-inherited?
   (lambda (att-def)
     (not (attribute-definition-synthesized? att-def))))
 
 ; Record type for AST nodes. AST nodes have a reference to the evaluator state used for evaluating their
 ; attributes and rewrites, the AST rule they represent a context of, their parent, children, attribute
 ; instances and the attribute cache entries they influence.
 (define-record-type node
   (fields
    (mutable evaluator-state)
    (mutable ast-rule)
    (mutable parent)
    (mutable children)
    (mutable attributes)
    (mutable cache-influences))
   (opaque #t)(sealed #t)
   (protocol
    (lambda (new)
      (lambda (ast-rule parent children)
        (new
         #f
         ast-rule
         parent
         children
         (list)
         (list))))))
 
 ; INTERNAL FUNCTION: Is the AST node of a certain type or subtype thereof (must be no terminal, bud or list node)?
 (define node-instance-of?-2
   (lambda (node rule)
     (or
      (eq? (node-ast-rule node) rule)
      (memq (node-ast-rule node) (=subtypes rule)))))
 
 ; INTERNAL FUNCTION: Given a node, return whether it is a terminal or not.
 (define node-terminal?
   (lambda (n)
     (eq? (node-ast-rule n) 'terminal)))
 
 ; INTERNAL FUNCTION: Given a node, return whether it is a non-terminal or not.
 (define node-non-terminal?
   (lambda (n)
     (not (node-terminal? n))))
 
 ; INTERNAL FUNCTION: Given a node, return whether it is a list node or not.
 (define node-list-node?
   (lambda (n)
     (eq? (node-ast-rule n) 'list-node)))
 
 ; INTERNAL FUNCTION: Given a node, return whether it is a bud node or not.
 (define node-bud-node?
   (lambda (n)
     (eq? (node-ast-rule n) 'bud-node)))
 
 ; INTERNAL FUNCTION: Given a node, return its child-index if it has a parent, otherwise return #f.
 (define node-child-index?
   (lambda (n)
     (if (node-parent n)
         (let loop ((children (node-children (node-parent n)))
                    (pos 1))
           (if (eq? (car children) n)
               pos
               (loop (cdr children) (+ pos 1))))
         #f)))
 
 ; INTERNAL FUNCTION: Given a node find a certain child by name. If the node has
 ; no such child, return #f, otherwise the child.
 (define node-find-child
   (lambda (n context-name)
     (and (not (node-list-node? n))
          (not (node-bud-node? n))
          (not (node-terminal? n))
          (let loop ((contexts (cdr (ast-rule-production (node-ast-rule n))))
                     (children (node-children n)))
            (if (null? contexts)
                #f
                (if (eq? (symbol-context-name (car contexts)) context-name)
                    (car children)
                    (loop (cdr contexts) (cdr children))))))))
 
 ; INTERNAL FUNCTION: Given a node find a certain attribute associated with it. If the node
 ; has no such attribute, return #f, otherwise the attribute. TODO: delte
 (define node-find-attribute
   (lambda (n name)
     (find
      (lambda (att)
        (eq? (attribute-definition-name (attribute-instance-definition att)) name))
      (node-attributes n))))
 
 ; INTERNAL FUNCTION: Given a node return a certain of its attributes if it exists otherwise #f.
 (define node-find-attribute-2
   (lambda (n name)
     (find
      (lambda (attribute-instance)
        (eq? (->name (attribute-instance-definition attribute-instance)) name))
      (node-attributes n))))
 
 ; INTERNAL FUNCTION: Given two nodes n1 and n2, return whether n1 is within the subtree spaned by n2 or not.
 (define node-inside-of?
   (lambda (n1 n2)
     (cond
       ((eq? n1 n2) #t)
       ((node-parent n1) (node-inside-of? (node-parent n1) n2))
       (else #f))))
 
 ; Record type for attribute instances of a certain attribute definition, associated with
 ; a certain node (context) and a cache.
 (define-record-type attribute-instance
   (fields (mutable definition) (mutable context) cache)
   (opaque #t)(sealed #t)
   (protocol
    (lambda (new)
      (lambda (definition context)
        (new definition context (make-hashtable equal-hash equal? 1))))))
 
 ; Record type for attribute cache entries. Attribute cache entries represent the values of
 ; and dependencies between attribute instances evaluated for certain arguments. The attribute
 ; instance of which an entry represents a value is called its context. If an entry already
 ; is evaluated, it caches the result of its context evaluated for its arguments. If an entry is
 ; not evaluated but its context is circular it stores an intermediate result of its fixpoint
 ; computation, called cycle value. Entries also track whether they are already in evaluation or
 ; not, such that the attribute evaluator can detect unexpected cycles.
 (define-record-type attribute-cache-entry
   (fields
    (mutable context)
    (mutable arguments)
    (mutable value)
    (mutable cycle-value)
    (mutable entered?)
    (mutable node-dependencies)
    (mutable cache-dependencies)
    (mutable cache-influences))
   (opaque #t)(sealed #t)
   (protocol
    (lambda (new)
      (lambda (att arguments) ; att: The attribute instance for which to construct a cache entry
        (new
         att
         arguments
         racr-nil
         (let ((circular? (attribute-definition-circularity-definition (attribute-instance-definition att))))
           (if circular?
               (car circular?)
               racr-nil))
         #f
         (list)
         (list)
         (list))))))
 
 ; Record type representing the internal state of RACR systems throughout their execution, i.e., while
 ; evaluating attributes and rewriting ASTs. An evaluator state consists of a flag indicating if the AG
 ; currently performs a fix-point evaluation, a flag indicating if throughout a fix-point iteration the
 ; value of an attribute changed and an attribute evaluation stack used for dependency tracking.
 (define-record-type evaluator-state
   (fields (mutable ag-in-cycle?) (mutable ag-cycle-change?) (mutable evaluation-stack))
   (opaque #t)(sealed #t)
   (protocol
    (lambda (new)
      (lambda ()
        (new #f #f (list))))))
 
 ; INTERNAL FUNCTION: Given an evaluator state, return whether it represents an evaluation in progress or
 ; not. If it represents an evaluation in progress return the current attribute in evaluation, otherwise #f.
 (define evaluator-state-in-evaluation?
   (lambda (state)
     (and (not (null? (evaluator-state-evaluation-stack state))) (car (evaluator-state-evaluation-stack state)))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Support API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; INTERNAL FUNCTION: Given an arbitrary Scheme entity, construct a string
 ; representation of it using display.
 (define object->string
   (lambda (x)
     (call-with-string-output-port
      (lambda (port)
        (display x port)))))
 
 (define-condition-type racr-exception &violation make-racr-exception racr-exception?)
 
 ; INTERNAL FUNCTION: Given an arbitrary sequence of strings and other Scheme entities, concatenate them to
 ; form an error message and throw a special RACR exception with the constructed message. Any entity that is
 ; not a string is treated as error information embedded in the error message between '[' and ']' characters,
 ; whereby the actual string representation of the entity is obtained using object->string.
 (define-syntax throw-exception
   (syntax-rules ()
     ((_ m-part ...)
      (raise-continuable
       (condition
        (make-racr-exception)
        (make-message-condition
         (string-append
          "RACR exception: "
          (let ((m-part* m-part))
            (if (string? m-part*)
                m-part*
                (string-append "[" (object->string m-part*) "]"))) ...)))))))
 
 ; INTERNAL FUNCTION: Procedure sequentially applying a function on all the AST rules of a set of rules which
 ; inherit, whereby supertypes are processed before their subtypes.
 (define apply-wrt-ast-inheritance
   (lambda (func rules)
     (let loop ((resolved ; The set of all AST rules that are already processed....
                 (filter ; ...Initially it consists of all the rules that have no supertypes.
                  (lambda (rule)
                    (not (ast-rule-supertype? rule)))
                  rules))
                (to-check ; The set of all AST rules that still must be processed....
                 (filter ; ...Initially it consists of all the rules that have supertypes.
                  (lambda (rule)
                    (ast-rule-supertype? rule))
                  rules)))
       (let ((to-resolve ; ...Find a rule that still must be processed and...
              (find
               (lambda (rule)
                 (memq (ast-rule-supertype? rule) resolved)) ; ...whose supertype already has been processed....
               to-check)))
         (when to-resolve ; ...If such a rule exists,...
           (func to-resolve) ; ...process it and...
           (loop (cons to-resolve resolved) (remq to-resolve to-check))))))) ; ...recur.
 
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
                 (#,(datum->syntax #'k 'specification->phase)
                  (lambda ()
                    (specification->phase spec*)))
                 (#,(datum->syntax #'k 'specify-attribute)
                  (lambda (att-name non-terminal index cached? equation circ-def)
                    (specify-attribute spec* att-name non-terminal index cached? equation circ-def))))
            (let-syntax ((#,(datum->syntax #'k 'ag-rule)
                          (syntax-rules ()
                            ((_ attribute-name definition (... ...))
                             (specify-ag-rule spec* attribute-name definition (... ...))))))
              body ...))))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Abstract Syntax Tree Specification ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (define specify-ast-rule
   (lambda (spec rule)
     ;;; Ensure, that the RACR system is in the correct specification phase:
     (when (> (racr-specification-specification-phase spec) 1)
       (throw-exception
        "Unexpected AST rule " rule "; "
        "AST rules can only be defined in the AST specification phase."))
     (letrec* ((ast-rule ; The parsed AST rule that will be added to the given specification.
                (make-ast-rule
                 spec
                 rule
                 racr-nil
                 racr-nil))
               (rule-string (symbol->string rule)) ; String representation of the encoded rule (used for parsing)
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
                      (throw-exception
                       "Unexpected end of AST rule " rule ";"
                       "Expected " c " character.")
                      (if (char=? (my-peek-char) c)
                          (set! pos (+ pos 1))
                          (throw-exception
                           "Invalid AST rule " rule "; "
                           "Unexpected " (my-peek-char) " character.")))))
               ; Support function parsing a symbol, i.e., retrieving its name, type, if it is a list and optional context name.
               (parse-symbol
                (lambda (location) ; location: l-hand, r-hand 
                  (let ((symbol-type (if (eq? location 'l-hand) "non-terminal" "terminal")))
                    (when (eos?)
                      (throw-exception
                       "Unexpected end of AST rule " rule "; "
                       "Expected " symbol-type "."))
                    (let* ((parse-name
                            (lambda (terminal?)
                              (let ((name
                                     (append
                                      (let loop ((chars (list)))
                                        (if (and (not (eos?)) (char-alphabetic? (my-peek-char)))
                                            (begin
                                              (when (and terminal? (not (char-lower-case? (my-peek-char))))
                                                (throw-exception
                                                 "Invalid AST rule " rule "; "
                                                 "Unexpected " (my-peek-char) " character."))
                                              (loop (cons (my-read-char) chars)))
                                            (reverse chars)))
                                      (let loop ((chars (list)))
                                        (if (and (not (eos?)) (char-numeric? (my-peek-char)))
                                            (loop (cons (my-read-char) chars))
                                            (reverse chars))))))
                                (when (null? name)
                                  (throw-exception
                                   "Unexpected " (my-peek-char) " character in AST rule " rule "; "
                                   "Expected " symbol-type "."))
                                (unless (char-alphabetic? (car name))
                                  (throw-exception
                                   "Malformed name in AST rule " rule "; "
                                   "Names must start with a letter."))
                                name)))
                           (terminal? (char-lower-case? (my-peek-char)))
                           (name (parse-name terminal?))
                           (kleene?
                            (and
                             (not terminal?)
                             (eq? location 'r-hand)
                             (not (eos?))
                             (char=? (my-peek-char) #\*)
                             (my-read-char)))
                           (context-name?
                            (and
                             (not terminal?)
                             (eq? location 'r-hand)
                             (not (eos?))
                             (char=? (my-peek-char) #\<)
                             (my-read-char)
                             (parse-name #f)))
                           (name-string (list->string name))
                           (name-symbol (string->symbol name-string)))
                      (when (and terminal? (eq? location 'l-hand))
                        (throw-exception
                         "Unexpected " name " terminal in AST rule " rule "; "
                         "Left hand side symbols must be non-terminals."))
                      (make-production-symbol
                       name-symbol
                       ast-rule
                       (not terminal?)
                       kleene?
                       (if context-name?
                           (string->symbol (list->string context-name?))
                           (if kleene?
                               (string->symbol (string-append name-string "*"))
                               name-symbol))
                       (list))))))
               (l-hand (parse-symbol 'l-hand)); The rule's l-hand
               (supertype ; The rule's super-type
                (and (not (eos?)) (char=? (my-peek-char) #\:) (my-read-char) (symbol-name (parse-symbol 'l-hand)))))
              (match-char! #\-)
              (match-char! #\>)
              (ast-rule-production-set!
               ast-rule
               (append
                (list l-hand)
                (let loop ((r-hand
                            (if (not (eos?))
                                (list (parse-symbol 'r-hand))
                                (list))))
                  (if (eos?)
                      (reverse r-hand)
                      (begin
                        (match-char! #\-)
                        (loop (cons (parse-symbol 'r-hand) r-hand)))))))
              (ast-rule-supertype?-set!
               ast-rule
               supertype)
              ; Check, that the rule's l-hand is not already defined:
              (when (racr-specification-find-rule spec (symbol-name l-hand))
                (throw-exception
                 "Invalid AST rule " rule "; "
                 "Redefinition of " (symbol-name l-hand) "."))
              (hashtable-set! ; Add the rule to the RACR specification.
               (racr-specification-rules-table spec)
               (symbol-name l-hand)
               ast-rule))))
 
 (define (specify-ast-rule-2 spec rule)
   (define rule-string (symbol->string rule)) ; The characters to parse.
   (define rule-string-length (string-length rule-string)) ; The number of characters to parse.
   (define pos 0) ; The current parsing position.
   
   (define peek-char ; Return the next character if it satisfies optional constraints and exists.
     (lambda constraints
       (define char-read? (and (< pos rule-string-length) (string-ref rule-string pos)))
       (and char-read? (for-all (lambda (f) (f char-read?)) constraints) char-read?)))
   
   (define read-char ; Similar to peek-char but additionally increments the parsing position.
     (lambda constraints
       (define char-read? (apply peek-char constraints))
       (unless char-read?
         (throw-exception "Invalid AST rule " rule ";"))
       (set! pos (+ pos 1))
       char-read?))
   
   (define (char= to-read) ; Construct filter for certain character that can be used by peek- and read-char.
     (lambda (char-read)
       (char=? char-read to-read)))
   
   (define (parse-rule) ; Parse complete rule.
     (define name #f)
     (define supertype? #f)
     (define rhand #f)
     (unless (peek-char char-upper-case?)
       (throw-exception "Invalid AST rule " rule ";"))
     (set! name (parse-identifier))
     (when (peek-char (char= #\:))
       (read-char)
       (unless (peek-char char-upper-case?)
         (throw-exception "Invalid AST rule " rule ";"))
       (set! supertype? (parse-identifier)))
     (read-char (char= #\-))
     (read-char (char= #\>))
     (set!
      rhand
      (if (peek-char char-alphabetic?)
          (let loop ()
            (let ((symbol (parse-symbol)))
              (if (peek-char (char= #\-))
                  (begin
                    (read-char)
                    (cons symbol (loop)))
                  (list symbol))))
          (list)))
     (when (peek-char)
       (throw-exception "Invalid AST rule " rule ";"))
     (create-ast ast-language 'AstRule (list name supertype? (create-ast-list rhand))))
   
   (define (parse-symbol) ; Parse right hand symbol including optional klenee closure and context name.
     (define non-terminal? (peek-char char-upper-case?))
     (define name (parse-identifier))
     (define klenee? (and non-terminal? (peek-char (char= #\*)) (read-char)))
     (define contextname? (and non-terminal? (peek-char (char= #\<)) (read-char) (parse-identifier)))
     (create-ast ast-language 'Symbol (list name klenee? contextname?)))
   
   (define (parse-identifier) ; Parse ordinary identifier, i.e., [a-zA-Z][a-zA-Z0-9]*
     (let loop ((id (list (read-char char-alphabetic?))))
       (let ((next-char? (peek-char)))
         (if (and next-char? (or (char-alphabetic? next-char?) (char-numeric? next-char?)))
             (loop (cons (read-char) id))
             (string->symbol (apply string (reverse id)))))))
   
   ;;; Before adding the AST rule, ensure that...
   (unless (symbol? rule) ; ...the given argument is of the expected type and...
     (throw-exception
      "Invalid AST rule definition;"
      "Wrong argument type (expected symbol encoding rule, but the argument was " rule "."))
   (when (> (racr-specification-2-specification-phase spec) 1) ; ...the language is in the correct specification phase.
     (throw-exception
      "Unexpected AST rule " rule ";"
      "AST rules can only be defined in the specification phase."))
   ;;; Parse the rule, add it to the RACR specification and return it:
   (let ((rule (parse-rule)))
     (rewrite-add (->astrules (racr-specification-2-ast-scheme spec)) rule)
     rule))
 
 (define (specify-start-symbol-2 spec start-symbol)
   ;;; Before changing the start symbol, ensure that...
   (unless (symbol? start-symbol) ; ...the given argument is of the expected type and...
     (throw-exception
      "Invalid start symbol definition;"
      "Wrong argument type (expected symbol encoding non-terminal, but the argument was " start-symbol ")."))
   (when (> (racr-specification-2-specification-phase spec) 1) ; ...the language is in the correct specification phase.
     (throw-exception
      "Unexpected " start-symbol " start symbol definition;"
      "The start symbol can only be defined in the specification phase."))
   ;;; Set the start symbol:
   (rewrite-terminal 'startsymbol (racr-specification-2-ast-scheme spec) start-symbol))
 
 (define (specify-attribute-2 spec name rule position cached? equation circularity-definition)
   ;;; Before adding the attribute definition, ensure that...
   (define wrong-argument-type ; ...the given arguments are of the expected type and...
     (or
      (and (not (symbol? name))
           (cons "symbol encoding attribute name" name))
      (and (not (symbol? rule))
           (cons "symbol denoting AST rule" rule))
      (and (not (symbol? position))
           (cons "symbol denoting AST rule context" position))
      (and (not (procedure? equation))
           (cons "function encoding attribute equation" equation))
      (and circularity-definition
           (or
            (not (pair? circularity-definition))
            (not (procedure? (cdr circularity-definition))))
           (cons "#f or (bottom-value equivalence-function) pair encoding circularity definition" circularity-definition))))
   (when wrong-argument-type
     (throw-exception
      "Invalid attribute definition; "
      "Wrong argument type (expected " (car wrong-argument-type) ", but the argument was " (cdr wrong-argument-type) ")."))
   (when (> (racr-specification-2-specification-phase spec) 1) ; ...the language is in the correct specification phase.
     (throw-exception
      "Unexpected " name " attribute definition; "
      "Attributes can only be defined in the specification phase."))
   ;;; Add the attribute to the RACR specification and return it:
   (let ((attribute
          (create-ast
           ast-language
           'Attribute
           (list name (cons rule position) equation circularity-definition cached?))))
     (rewrite-add (->attribution (racr-specification-2-ast-scheme spec)) attribute)
     attribute))
 
 (define (compile-specification-2 spec)
   (define ast-scheme (racr-specification-2-ast-scheme spec))
   ;;; Before compiling the specification, ensure that...
   (when (> (racr-specification-2-specification-phase spec) 1) ; ...it is in the correct specification phase and...
     (throw-exception
      "Unexpected RACR specification compilation;"
      "The specification already has been compiled."))
   (unless (=well-formed? ast-scheme) ; ...well-formed.
     (throw-exception
      "Cannot compile RACR specification;"
      "The specification is not well-formed."))
   ;;; Compile the specification, i.e.,...
   (racr-specification-2-specification-phase-set! spec 2) ; ...proceed to the next specifcation phase and...
   (for-each ; ...precompute the attribution factories of all possible context switches. The attribution for...
    (lambda (rule)
      (define abstracted-context (list #f #f (->name rule)))
      (=attribution-factory ast-scheme #f abstracted-context) ; ...new nodes and...
      (for-each ; ...all their...
       (lambda (sub)
         (define refined-context (list #f #f (->name sub)))
         (=attribution-factory ast-scheme abstracted-context refined-context) ; ...refinements and...
         (=attribution-factory ast-scheme refined-context abstracted-context)) ; ...abstractions and...
       (=subtypes rule))
      (for-each ; ...all their children,...
       (lambda (symbol)
         (for-each ; ...of all permitted kind, whether...
          (lambda (child-rule)
            (define without-parent-context (list #f #f (->name child-rule)))
            (define with-parent-context (list (->name rule) (=contextname symbol) (->name child-rule)))
            (=attribution-factory ast-scheme without-parent-context with-parent-context) ; ...added,...
            (=attribution-factory ast-scheme with-parent-context without-parent-context) ; ...removed,...
            (for-each
             (lambda (child-sub)
               (define refined-child-context (list (->name rule) (=contextname symbol) (->name child-sub)))
               (=attribution-factory ast-scheme with-parent-context refined-child-context); ...refined or...
               (=attribution-factory ast-scheme refined-child-context with-parent-context)); ...abstracted.
             (=subtypes child-rule)))
          (let ((child-rule (=non-terminal? symbol)))
            (cons child-rule (=subtypes child-rule)))))
       (=rhand-non-terminal-symbols rule)))
    (=ast-rules ast-scheme)))
 
 (define-record-type racr-specification-2
   (fields (mutable specification-phase) ast-scheme)
   (opaque #t)(sealed #t)
   (protocol
    (lambda (new)
      (lambda ()
        (new
         1
         (with-specification
          ast-language
          (create-ast
           'AstScheme ; The initial AST scheme just consists of...
           (list
            (create-ast-list
             (list
              (create-ast
               'AstRule ; ...the error rule with...
               (list
                racr-nil
                #f
                (create-ast-list
                 (list
                  (create-ast 'Symbol (list 'error-symbol #f 'error-symbol)))))))) ; ...the error symbol.
            #f
            (create-ast-list (list))))))))))
 
 (define ast-language (make-racr-specification))
 
 ; AST Accessors:
 (define (->astrules n) (ast-child 'astrules n))
 (define (->attribution n) (ast-child 'attribution n))
 (define (->rhand n) (ast-child 'rhand n))
 (define (->startsymbol n) (ast-child 'startsymbol n))
 (define (->name n) (ast-child 'name n))
 (define (->supertype n) (ast-child 'supertype n))
 (define (->klenee n) (ast-child 'klenee n))
 (define (->contextname n) (ast-child 'contextname n))
 (define (->context n) (ast-child 'context n))
 (define (->equation n) (ast-child 'equation n))
 (define (->circularitydefinition n) (ast-child 'circularitydefinition n))
 (define (->cached n) (ast-child 'cached n))
 (define (->* n) (ast-children n))
 
 ; Attribute Accessors:
 (define (=error-rule n) (att-value 'error-rule n))
 (define (=error-symbol n) (att-value 'error-symbol n))
 (define (=error-node? n) (att-value 'error-node? n))
 (define (=attribute-definitions n) (att-value 'attribute-definitions n))
 (define (=ast-rules n) (att-value 'ast-rules n))
 (define (=containing-rule n) (att-value 'containing-rule n))
 (define (=terminal? n) (att-value 'terminal? n))
 (define (=contextname n) (att-value 'contextname n))
 (define (=lookup-rule n name) (att-value 'lookup-rule n name))
 (define (=startsymbol n) (att-value 'startsymbol n))
 (define (=supertype? n) (att-value 'supertype? n))
 (define (=non-terminal? n) (att-value 'non-terminal? n))
 (define (=lookup-contextname n name) (att-value 'lookup-contextname n name))
 (define (=supertypes n) (att-value 'supertypes n))
 (define (=subtypes n) (att-value 'subtypes n))
 (define (=expanded-rhand n) (att-value 'expanded-rhand n))
 (define (=rhand-non-terminal-symbols n) (att-value 'rhand-non-terminal-symbols n))
 (define (=productive? n) (att-value 'productive? n))
 (define (=direct-derivable n) (att-value 'direct-derivable n))
 (define (=derivable n) (att-value 'derivable n))
 (define (=synthesised? n) (att-value 'synthesised? n))
 (define (=inherited? n) (att-value 'inherited? n))
 (define (=circular? n) (att-value 'circular? n))
 (define (=bottom-value n) (att-value 'bottom-value n))
 (define (=equality-function n) (att-value 'equality-function n))
 (define (=context-rule n) (att-value 'context-rule n))
 (define (=context n) (att-value 'context n))
 (define (=attributes-for-rule/symbol n) (att-value 'attributes-for-rule/symbol n))
 (define =attributes
   (case-lambda
     ((n) (att-value 'attributes n))
     ((n context-name) (att-value 'attributes n context-name))))
 (define (=attributes-for-context n parent-type? context-name? child-type)
   (att-value 'attributes-for-context n parent-type? context-name? child-type))
 (define (=well-formed? n) (att-value 'well-formed? n))
 (define (=local-correct? n) (att-value 'local-correct? n))
 (define =ast-node-factory
   (case-lambda
     ((n) (att-value 'ast-node-factory n))
     ((n name) (att-value 'ast-node-factory n name))))
 (define (=context-checker n) (att-value 'context-checker n))
 (define (=context-factory n) (att-value 'context-factory n))
 (define (=attribution-factory n old-context new-context)
   (att-value 'attribution-factory n old-context new-context))
 
 (define (load-ast-language)
   (define set-union
     (case-lambda
       ((s1 s2 f) ; Given two sets s1 and s2 and an equality function f, return the set union of s1 and s2 w.r.t. f.
        (fold-left
         (lambda (result e2)
           (if (find (lambda (e1) (f e1 e2)) s1)
               result
               (cons e2 result)))
         s1
         s2))
       ((s1 s2) ; Given two sets s1 and s2, return the set union of s1 and s2 (use eq? to compare elements).
        (set-union s1 s2 eq?))))
   
   (define-record-type attribute-instance-2
     (fields name cached? (mutable definition) (mutable context) cache)
     (opaque #t)(sealed #t)
     (protocol
      (lambda (new)
        (lambda (name cached? definition context)
          (new name cached? definition context (make-hashtable equal-hash equal? 1))))))
   
   ; Meta-instantiation contract: AST rule specification, attribute specification,
   ;    start symbol specification and specification compilation only query attributes
   ;    and no further AST information. All such queries are cache hits.
   (let* ((% racr-nil) ; Never queried information.
          (dummy-attribute-instance
           (lambda (name . entries)
             (let* ((instance (make-attribute-instance-2 name #t % %))
                    (cache (attribute-instance-2-cache instance)))
               (for-each (lambda (entry) (hashtable-set! cache (car entry) (cdr entry))) entries)
               instance)))
          (dummy-node (lambda () (make-node % % %)))
          
          (AstScheme                          (dummy-node))
          (AstScheme-astrules                 (dummy-node))
          (AstScheme-startsymbol              (dummy-node))
          (AstScheme-attribution              (dummy-node))
          (AstRule                            (dummy-node))
          (AstRule-name                       (dummy-node))
          (AstRule-supertype                  (dummy-node))
          (AstRule-rhand                      (dummy-node))
          (Symbol                             (dummy-node))
          (Symbol-name                        (dummy-node))
          (Symbol-klenee                      (dummy-node))
          (Symbol-contextname                 (dummy-node))
          (Attribute                          (dummy-node))
          (Attribute-name                     (dummy-node))
          (Attribute-context                  (dummy-node))
          (Attribute-equation                 (dummy-node))
          (Attribute-circularitydefinition    (dummy-node))
          (Attribute-cached                   (dummy-node))
          
          (error-node?->f                     (dummy-attribute-instance 'error-node? (cons (list) #f)))
          
          (klenee?->t                         (dummy-attribute-instance 'klenee? (cons (list) #t)))
          (klenee?->f                         (dummy-attribute-instance 'klenee? (cons (list) #f)))
          
          (contextname->name                  (dummy-attribute-instance 'contextname (cons (list) 'name)))
          (contextname->astrules              (dummy-attribute-instance 'contextname (cons (list) 'astrules)))
          (contextname->startsymbol           (dummy-attribute-instance 'contextname (cons (list) 'startsymbol)))
          (contextname->attribution           (dummy-attribute-instance 'contextname (cons (list) 'attribution)))
          (contextname->astrules              (dummy-attribute-instance 'contextname (cons (list) 'astrules)))
          (contextname->supertype             (dummy-attribute-instance 'contextname (cons (list) 'supertype)))
          (contextname->rhand                 (dummy-attribute-instance 'contextname (cons (list) 'rhand)))
          (contextname->klenee                (dummy-attribute-instance 'contextname (cons (list) 'klenee)))
          (contextname->contextname           (dummy-attribute-instance 'contextname (cons (list) 'contextname)))
          (contextname->context               (dummy-attribute-instance 'contextname (cons (list) 'context)))
          (contextname->equation              (dummy-attribute-instance 'contextname (cons (list) 'equation)))
          (contextname->circularitydefinition (dummy-attribute-instance 'contextname (cons (list) 'circularitydefinition)))
          (contextname->cached                (dummy-attribute-instance 'contextname (cons (list) 'cached)))
          
          (terminal?->t                       (dummy-attribute-instance 'terminal? (cons (list) #t)))
          (terminal?->f                       (dummy-attribute-instance 'terminal? (cons (list) #f)))
          
          (non-terminal?->AstRule             (dummy-attribute-instance 'non-terminal? (cons (list) AstRule)))
          (non-terminal?->Attribute           (dummy-attribute-instance 'non-terminal? (cons (list) Attribute)))
          (non-terminal?->Symbol              (dummy-attribute-instance 'non-terminal? (cons (list) Symbol)))
          
          (name->AstScheme                    (dummy-attribute-instance 'name (cons (list) 'AstScheme)))
          (name->AstRule                      (dummy-attribute-instance 'name (cons (list) 'AstRule)))
          (name->Symbol                       (dummy-attribute-instance 'name (cons (list) 'Symbol)))
          (name->Attribute                    (dummy-attribute-instance 'name (cons (list) 'Attribute)))
          
          (lookup-rule
           (dummy-attribute-instance
            'lookup-rule
            (cons (list 'AstScheme) AstScheme)
            (cons (list 'AstRule) AstRule)
            (cons (list 'Symbol) Symbol)
            (cons (list 'Attribute) Attribute)))
          
          (AstScheme:attributes
           (dummy-attribute-instance
            'attributes
            (cons (list) (list))))
          
          (AstRule:attributes
           (dummy-attribute-instance
            'attributes
            (cons (list) (list))))
          
          (Symbol:attributes
           (dummy-attribute-instance
            'attributes
            (cons (list) (list))))
          
          (Attribute:attributes
           (dummy-attribute-instance
            'attributes
            (cons (list) (list))))
          
          (AstScheme:expanded-rhand
           '(dummy-attribute-instance
             'expanded-rhand
             (cons (list)
                   (list
                    AstScheme-astrules
                    AstScheme-startsymbol
                    AstScheme-attribution))))
          
          (AstRule:expanded-rhand
           '(dummy-attribute-instance
             'expanded-rhand
             (cons (list)
                   (list
                    AstRule-name
                    AstRule-supertype
                    AstRule-rhand))))
          
          (Symbol:expanded-rhand
           '(dummy-attribute-instance
             'expanded-rhand
             (cons (list)
                   (list
                    Symbol-name
                    Symbol-klenee
                    Symbol-contextname))))
          
          (Attribute:expanded-rhand
           '(dummy-attribute-instance
             'expanded-rhand
             (cons (list)
                   (list
                    Attribute-name
                    Attribute-context
                    Attribute-equation
                    Attribute-circularitydefinition
                    Attribute-cached))))
          
          (attributes-for-context
           (dummy-attribute-instance
            'attributes-for-context
            (cons (list 'AstScheme 'astrules 'AstRule) (list))
            (cons (list 'AstScheme 'attribution 'Attribute) (list))
            (cons (list 'AstRule 'rhand 'Symbol) (list))))
          )
     
     (node-attributes-set! AstScheme (list error-node?->f name->AstScheme AstScheme:expanded-rhand lookup-rule attributes-for-context))
     (node-attributes-set! AstRule   (list error-node?->f name->AstRule AstRule:expanded-rhand))
     (node-attributes-set! Symbol    (list error-node?->f name->Symbol Symbol:expanded-rhand))
     (node-attributes-set! Attribute (list error-node?->f name->Attribute Attribute:expanded-rhand))
     
     (node-attributes-set! AstScheme-astrules              (list terminal?->f klenee?->t contextname->astrules non-terminal?->AstRule))
     (node-attributes-set! AstScheme-startsymbol           (list terminal?->t klenee?->f contextname->startsymbol))
     (node-attributes-set! AstScheme-attribution           (list terminal?->f klenee?->t contextname->attribution non-terminal?->Attribute))
     (node-attributes-set! AstRule-name                    (list terminal?->t klenee?->f contextname->name))
     (node-attributes-set! AstRule-supertype               (list terminal?->t klenee?->f contextname->supertype))
     (node-attributes-set! AstRule-rhand                   (list terminal?->f klenee?->t contextname->rhand non-terminal?->Symbol))
     (node-attributes-set! Symbol-name                     (list terminal?->t klenee?->f contextname->name))
     (node-attributes-set! Symbol-klenee                   (list terminal?->t klenee?->f contextname->klenee))
     (node-attributes-set! Symbol-contextname              (list terminal?->t klenee?->f contextname->contextname))
     (node-attributes-set! Attribute-name                  (list terminal?->t klenee?->f contextname->name))
     (node-attributes-set! Attribute-context               (list terminal?->t klenee?->f contextname->context))
     (node-attributes-set! Attribute-equation              (list terminal?->t klenee?->f contextname->equation))
     (node-attributes-set! Attribute-circularitydefinition (list terminal?->t klenee?->f contextname->circularitydefinition))
     (node-attributes-set! Attribute-cached                (list terminal?->t klenee?->f contextname->cached))
     
     )
   
   (with-specification
    ast-language
    
    ;;; AST Scheme:
    
    (ast-rule 'AstScheme->AstRule*<astrules-startsymbol-Attribute*<attribution)
    (ast-rule 'AstRule->name-supertype-Symbol*<rhand)
    (ast-rule 'Symbol->name-klenee-contextname)
    (ast-rule 'Attribute->name-context-equation-circularitydefinition-cached)
    
    (compile-ast-specifications 'AstScheme)
    
    ;;; AST Query Support:
    
    (ag-rule
     error-rule ; Childless, non-inheriting rule with invalid name referd to by undeclared rule references.
     (AstScheme
      (lambda (n)
        (=lookup-rule n racr-nil))))
    
    (ag-rule
     error-symbol ; Terminal symbol with invalid context name refered to by undeclared symbol references.
     (AstScheme
      (lambda (n)
        (=lookup-contextname (=error-rule n) 'error-symbol))))
    
    (ag-rule
     error-node? ; Is the rule/symbol the error rule/symbol?
     
     (AstRule (lambda (n) (eq? n (=error-rule n))))
     (Symbol (lambda (n) (eq? n (=error-symbol n)))))
    
    (ag-rule
     attribute-definitions ; List of all attribute definitions. 
     (AstScheme
      (lambda (n)
        (->* (->attribution n)))))
    
    (ag-rule
     ast-rules ; List of all AST rules.
     (AstScheme
      (lambda (n)
        (->* (->astrules n)))))
    
    (ag-rule
     containing-rule ; Broadcast the AST rule containing a symbol.
     (AstRule (lambda (n) n)))
    
    (ag-rule
     terminal? ; Is the symbol a terminal?
     (Symbol
      (lambda (n)
        (char-lower-case? (string-ref (symbol->string (->name n)) 0)))))
    
    (ag-rule
     contextname ; User-specified, and otherwise implicit, context name of the symbol.
     (Symbol
      (lambda (n)
        (or
         (->contextname n)
         (if (->klenee n)
             (string->symbol (string-append (symbol->string (->name n)) "*"))
             (->name n))))))
    
    ;;; Name Analysis:
    
    (ag-rule
     lookup-rule ; Given a symbolic name, find the respective AST rule (error rule if not defined).
     (AstScheme
      (lambda (n name)
        (or
         (ast-find-child
          (lambda (i n)
            (eq? (->name n) name))
          (->astrules n))
         (=error-rule n)))))
    
    (ag-rule
     startsymbol ; The grammar's start rule (error rule if not defined).
     (AstScheme
      (lambda (n)
        (=lookup-rule n (->startsymbol n)))))
    
    (ag-rule
     supertype? ; The rule's supertype (error rule if not defined) or #f if the rule does not inherite.
     (AstRule
      (lambda (n)
        (and
         (->supertype n)
         (=lookup-rule n (->supertype n))))))
    
    (ag-rule
     non-terminal? ; The symbol's defining rule (error rule if not defined) or #f if the symbol is a terminal.
     (Symbol
      (lambda (n)
        (and
         (not (=terminal? n))
         (=lookup-rule n (->name n))))))
    
    (ag-rule
     lookup-contextname ; Given a symbolic name, find the respective child (error symbol if not defined).
     (AstRule
      (lambda (n name)
        (if (eq? name '*)
            n
            (or
             (find
              (lambda (n)
                (eq? (=contextname n) name))
              (=expanded-rhand n))
             (=error-symbol n))))))
    
    ;;; Inheritance Analysis:
    
    (ag-rule
     supertypes ; List of all supertypes ordered w.r.t. inheritance (supertypes before their subtypes).
     (AstRule
      (lambda (n)
        (reverse
         (let loop ((current-rule n))
           (let ((supertype? (=supertype? current-rule)))
             (cond
               ((or (not supertype?) (=error-node? supertype?))
                (list))
               ((eq? supertype? n)
                (list supertype?))
               (else (cons supertype? (loop supertype?))))))))))
    
    (ag-rule
     subtypes ; List of all subtypes (transitive but, if well-formed, not reflexive).
     (AstRule
      (lambda (n)
        (filter
         (lambda (rule)
           (memq n (=supertypes rule)))
         (=ast-rules n)))))
    
    (ag-rule
     expanded-rhand ; List of right-hand symbols including inherited ones.
     (AstRule
      (lambda (n)
        (fold-right
         (lambda (n result)
           (append
            (->* (->rhand n))
            result))
         (->* (->rhand n))
         (=supertypes n)))))
    
    (ag-rule
     rhand-non-terminal-symbols ; List of right-hand symbols that are non-terminals.
     (AstRule
      (lambda (n)
        (filter
         (lambda (n)
           (not (=terminal? n)))
         (=expanded-rhand n)))))
    
    ;;; Derivability Analysis:
    
    (ag-rule
     productive? ; Does there exist a finite AST whose root is typed with the rule?
     (AstRule
      (lambda (n)
        (for-all
            (lambda (n)
              (or
               (->klenee n)
               (=productive? (=non-terminal? n))))
          (=rhand-non-terminal-symbols n)))
      #f
      (lambda (r1 r2) (if r1 r2 (not r2)))))
    
    (ag-rule
     direct-derivable ; List of AST rules that are non-transitive applicable.
     (AstRule
      (lambda (n)
        (fold-left
         (lambda (result symbol)
           (let ((rule (=non-terminal? symbol)))
             (set-union
              (set-union
               (set-union result (=supertypes rule))
               (list rule))
              (=subtypes rule))))
         (list)
         (=rhand-non-terminal-symbols n)))))
    
    (ag-rule
     derivable ; List of AST rules that are transitive applicable.
     (AstRule
      (lambda (n)
        (fold-left
         (lambda (result rule)
           (set-union result (=derivable rule)))
         (=direct-derivable n)
         (=direct-derivable n)))
      (list)
      (lambda (r1 r2) (= (length r1) (length r2)))))
    
    ;;; Attribution Analysis:
    
    (ag-rule
     synthesised? ; Is the attribute synthesised?
     (Attribute
      (lambda (n)
        ;(ast-subtype? (=context n) 'AstRule)
        (eq? (cdr (->context n)) '*))))
    
    (ag-rule
     inherited? ; Is the attribute inherited?
     (Attribute
      (lambda (n)
        (not (=synthesised? n)))))
    
    (ag-rule
     circular? ; Is the attribute circular?
     (Attribute
      (lambda (n)
        (->circularitydefinition n))))
    
    (ag-rule
     bottom-value ; The attribute's bottom value if it is circular, exception otherwise.
     (Attribute
      (lambda (n)
        (car (->circularitydefinition n)))))
    
    (ag-rule
     equality-function ; The attribute's equality function if it is circular, exception otherwise.
     (Attribute
      (lambda (n)
        (cdr (->circularitydefinition n)))))
    
    (ag-rule
     context-rule ; AST rule in whose context the attribute is defined.
     (Attribute
      (lambda (n)
        (=lookup-rule n (car (->context n))))))
    
    (ag-rule
     context ; AST rule/symbol the attribute is defined for (AST rule if synthesised, symbol if inherited).
     (Attribute
      (lambda (n)
        (=lookup-contextname (=context-rule n) (cdr (->context n))))))
    
    (let ((attributes-for-node ; List of attribute definitions for AST node.
           (lambda (n)
             (filter
              (lambda (attribute)
                (eq? (=context attribute) n))
              (=attribute-definitions n)))))
      
      (ag-rule
       attributes-for-rule/symbol ; List of attribute definitions for the rule/symbol.
       
       (AstRule attributes-for-node)
       (Symbol attributes-for-node)))
    
    (ag-rule
     attributes ; List of attributes of AST rules and their right hand symbols considering inheritance.
     (AstRule
      (case-lambda
        ((n) ; List of synthesised attributes of the rule.
         (fold-right ; Process all ancestor AST rules in order of inheritance and...
          (lambda (n result)
            (set-union ; ...add the definitions of all synthesised attributes...
             result
             (=attributes-for-rule/symbol n)
             (lambda (a1 a2) ; ...that are not already defined.
               (eq? (->name a1) (->name a2)))))
          (=attributes-for-rule/symbol n)
          (=supertypes n)))
        ((n context-name) ; List of inherited attributes of a symbol of the rule.
         (let ((symbol (=lookup-contextname n context-name)))
           (fold-right ; Process each AST rule that has the symbol in order of inheritance and...
            (lambda (n result)
              (set-union ; ...add the definitions of all inherited attributes...
               result
               (filter
                (lambda (attribute)
                  (eq? (=context-rule attribute) n))
                (=attributes-for-rule/symbol symbol))
               (lambda (a1 a2) ; ...that are not already defined for the symbol.
                 (eq? (->name a1) (->name a2)))))
            (list)
            (memq (=containing-rule symbol) (append (=supertypes n) (list n)))))))))
    
    (ag-rule
     attributes-for-context ; Sorted list of all attributes of a node of a certain type if it is in a certain context.
     (AstScheme
      (lambda (n parent-type? context-name? child-type)
        (list-sort
         (lambda (a1 a2)
           (string<? (symbol->string (->name a1)) (symbol->string (->name a2))))
         (if parent-type?
             (set-union
              (=attributes (=lookup-rule n child-type))
              (=attributes (=lookup-rule n parent-type?) context-name?)
              (lambda (a1 a2)
                (eq? (->name a1) (->name a2))))
             (=attributes (=lookup-rule n child-type)))))))
    
    ;;; Well-formedness Analysis:
    
    (let ((well-formed?-visitor ; Is an AST node, and are all ASTs of certain children of it, local correct?
           (lambda (n . to-visit)
             (and
              (=local-correct? n)
              (for-all
                  (lambda (to-visit)
                    (not
                     (ast-find-child
                      (lambda (i n)
                        (not (=well-formed? n)))
                      to-visit)))
                to-visit)))))
      
      (ag-rule
       well-formed? ; Is the specification valid, such that attributed AST instances can be constructed?
       
       (AstScheme (lambda (n) (well-formed?-visitor n (->astrules n) (->attribution n))))
       (AstRule (lambda (n) (well-formed?-visitor n (->rhand n))))
       (Symbol well-formed?-visitor)
       (Attribute well-formed?-visitor)))
    
    (ag-rule
     local-correct? ; Is a certain part of the specification valid?
     
     (AstScheme
      (lambda (n)
        (not (=error-node? (=startsymbol n))))) ; The start rule is defined.
     
     (AstRule
      (lambda (n)
        (or ; Either,...
         (=error-node? n) ; ...the rule is the error rule or...
         (and
          (eq? (=lookup-rule n (->name n)) n) ; ...(1) its name is unique,...
          (let ((supertype? (=supertype? n))) ; ...(2) if it has a supertype it exists,...
            (or (not supertype?) (not (=error-node? supertype?))))
          (not (memq n (=subtypes n))) ; ...(3) if it inherits inheritance is cycle free,...
          (=productive? n) ; ...(4) it is productive and...
          (let ((s (=startsymbol n))) ; ...(4) either,...
            (or (memq n (cons s (=subtypes s))) ; ...the startsymbol, a subtype of the startsymbol or...
                (memq n (=derivable s)))))))) ; ...reachable from the startsymbol.
     
     (Symbol
      (lambda (n)
        (or ; Either,...
         (=error-node? n) ; ...the symbol is the error symbol or,...
         (let ((rule? (=non-terminal? n)))
           (and
            (not (and (->klenee n) (not rule?))) ; ...in case of Klenee closure, the symbol is a non-terminal,...
            (or (not rule?) (not (=error-node? rule?))) ; ...it is a terminal or a defined non-terminal and...
            (eq? (=lookup-contextname n (=contextname n)) n)))))) ; its context name is unique.
     
     (Attribute
      (lambda (n)
        (let ((context (=context n)))
          (and
           (not (=error-node? context)) ; The attribute's definition context exists and,...
           (not ; ...in case the attribute is inherited, is not a terminal and...
            (and (=inherited? n) (=terminal? context)))
           (eq? ; ...the definition is unique for it.
            (find (lambda (attribute) (eq? (->name attribute) (->name n)))
                  (if (=synthesised? n)
                      (=attributes context)
                      (=attributes (=context-rule n) (cdr (->context n)))))
            n))))))
    
    ;;; AST Construction:
    
    (ag-rule
     ast-node-factory ; Function that can be used to instantiate new AST nodes of certain type.
     
     (AstScheme
      (lambda (n name)
        (lambda children
          (let ((rule (=lookup-rule n name)))
            (when (=error-node? rule)
              (throw-exception ; BEWARE: Prolonged check ensures the rule exists at construction time!
               "Cannot construct " name " fragment;"
               "Unknown node type."))
            (apply (=ast-node-factory rule) children)))))
     
     (AstRule
      (lambda (n)
        (define new-parent-type (->name n)) ; TODO: Delete
        
        ;(define children-fit? ; Do the given children fit in context?
        ;  (let loop ((rhand (=expanded-rhand n)))
        ;    (if (null? rhand)
        ;        (lambda (children) (null? children))
        ;        (let ((fits? (=context-checker (car rhand)))
        ;              (loop (loop (cdr rhand))))
        ;          (lambda (children)
        ;            (and (not (null? children)) (fits? (car children)) (loop (cdr children))))))))
        ;(define prepare-children ; Prepare the given children for new context.
        ;  (let loop ((rhand (=expanded-rhand n)))
        ;    (if (null? rhand)
        ;        (lambda x (list))
        ;        (let ((prepare (=context-factory (car rhand)))
        ;              (loop (loop (cdr rhand))))
        ;          (lambda (new-parent children)
        ;            (cons (prepare new-parent-type new-parent (car children)) (loop new-parent (cdr children))))))))
        
        ;(define rhand (=expanded-rhand n))
        (define checkers (map =context-checker (=expanded-rhand n)))
        (define factories (map =context-factory (=expanded-rhand n)))
        (when (< (racr-specification-specification-phase (ast-specification n)) 2)
          (throw-exception ; BEWARE: Immediate check ensures the language specification is immutable!
           "Cannot construct fragment;"
           "The RACR specification is not compiled yet. It still is in the specification phase."))
        (lambda children
          (let ((new-fragment (make-node n #f (list))))
            ;;; Before constructing the fragment ensure, that...
            (unless (and ; ...the given children fit.
                     (= (length children) (length checkers))
                     (for-all (lambda (f c) (f c)) checkers children))
              (throw-exception
               "Cannot construct " n " fragment;"
               "The given children do not fit."))
            ;;; When all constraints are satisfied, construct the fragment, i.e.,...
            (node-children-set! new-fragment (map (lambda (f c) (f new-parent-type new-fragment c)) factories children)) ; ...add its children,...
            (distribute-evaluator-state (make-evaluator-state) new-fragment) ; ...distribute the fragment's evaluator state and...
            (node-attributes-set! ; ...initialize its synthesized attributes.
             new-fragment
             (map (lambda (attribute) (make-attribute-instance attribute new-fragment))
                  (=attributes n)))
            new-fragment))))) ; Finally, return the constructed fragment.
    
    (ag-rule
     context-checker ; Function deciding if a given Scheme entity can become child in certain context.
     (Symbol
      (lambda (n)
        (define expected-type? (=non-terminal? n))
        (define (satisfies-type? node)
          (or (node-bud-node? node) (node-instance-of?-2 node expected-type?)))
        (cond
          ((not expected-type?) ; Context is terminal?
           (lambda (node) #t))
          ((->klenee n) ; Context is list?
           (lambda (node)
             (and
              (can-be-non-terminal-child? node)
              (or
               (node-bud-node? node)
               (and
                (node-list-node? node)
                (for-all satisfies-type? (->* node)))))))
          (else ; Context is ordinary non-terminal!
           (lambda (node)
             (and
              (can-be-non-terminal-child? node)
              (not (node-list-node? node))
              (satisfies-type? node))))))))
    
    (ag-rule
     context-factory ; Internal function preparing a given Scheme entity to become child in certain context.
     (Symbol
      (lambda (n) ; TODO: Delete unnecessary new-parent-type
        (define context-name (=contextname n))
        (if (=terminal? n)
            (lambda (new-parent-type new-parent child)
              (make-node 'terminal new-parent child))
            (lambda (new-parent-type new-parent child)
              (for-each ; Flush all attribute cache entries depending on the child being a root,...
               (lambda (influence)
                 (flush-attribute-cache-entry (car influence)))
               (filter
                (lambda (influence)
                  (vector-ref (cdr influence) 1))
                (node-cache-influences child)))
              (node-parent-set! child new-parent) ; ...set its parent and...
              (update-attributes-2 new-parent-type context-name child) ; ...update its inherited attributes.
              child)))))
    
    (ag-rule
     attribution-factory ; Internal function updating the attribution of a node w.r.t. certain old and new context.
     (AstScheme
      (lambda (n old-context new-context)
        (define (equal-semantics a1 a2) ; Evaluate two attribute definitions always the same?
          (or ; Attribute definitions are semantically equivalent, if either...
           (eq? a1 a2) ; ...they are the same or...
           (and
            (eq? (->equation a1) (->equation a2)) ; ...have the same equation and...
            (if (=circular? a1) ; ...circularity definition.
                (and
                 (=circular? a2)
                 (equal? (=bottom-value a1) (=bottom-value a2))
                 (eq? (=equality-function a1) (=equality-function a2)))
                (not (=circular? a2))))))
        (let loop ((old-attribution (if old-context (apply =attributes-for-context n old-context) (list)))
                   (new-attribution (apply =attributes-for-context n new-context)))
          (cond
            ((and (null? old-attribution) (null? new-attribution)) ; No attributes to process left:
             (lambda (n old-instances)
               (list)))
            ((null? new-attribution) ; Only old attributes to delete left:
             (let ((loop (loop (cdr old-attribution) new-attribution)))
               (lambda (n old-instances)
                 (flush-attribute-instance (car old-instances))
                 (attribute-instance-context-set! (car old-instances) racr-nil)
                 (loop n (cdr old-instances)))))
            ((null? old-attribution) ; Only new attributes to add left:
             (let ((new-attribute (car new-attribution))
                   (loop (loop old-attribution (cdr new-attribution))))
               (lambda (n old-instances)
                 (cons (make-attribute-instance new-attribute n) (loop n #f)))))
            (else ; New and old attributes left:
             (let ((a1 (car old-attribution))
                   (a2 (car new-attribution)))
               (cond
                 ((eq? (->name a1) (->name a2)) ; Definitions for the same attribute, either...
                  (let ((loop (loop (cdr old-attribution) (cdr new-attribution))))
                    (if (equal-semantics a1 a2)
                        (lambda (n old-instances) ; ...semantic equivalent or...
                          (attribute-instance-definition-set! (car old-instances) a2)
                          (cons (car old-instances) (loop n (cdr old-instances))))
                        (lambda (n old-instances) ; ...not semantic equivalent.
                          (flush-attribute-instance (car old-instances))
                          (attribute-instance-definition-set! (car old-instances) a2)
                          (cons (car old-instances) (loop n (cdr old-instances)))))))
                 ((string<? (symbol->string (->name a1)) ; Old attribute deleted from lexical sorted list.
                            (symbol->string (->name a2)))
                  (let ((loop (loop (cdr old-attribution) new-attribution)))
                    (lambda (n old-instances)
                      (flush-attribute-instance (car old-instances))
                      (attribute-instance-context-set! (car old-instances) racr-nil)
                      (loop n (cdr old-instances)))))
                 (else ; New attribute inserted into lexical sorted list.
                  (let ((loop (loop old-attribution (cdr new-attribution))))
                    (lambda (n old-instances)
                      (cons (make-attribute-instance a2 n) (loop n old-instances)))))))))))))
    
    (compile-ag-specifications)))
 
 (define compile-ast-specifications
   (lambda (spec start-symbol)
     ;;; Ensure, that the RACR system is in the correct specification phase and...
     (let ((current-phase (racr-specification-specification-phase spec)))
       (if (> current-phase 1)
           (throw-exception
            "Unexpected AST compilation; "
            "The AST specifications already have been compiled.")
           ; ...iff so proceed to the next specification phase:
           (racr-specification-specification-phase-set! spec (+ current-phase 1))))
     
     (racr-specification-start-symbol-set! spec start-symbol)
     (let* ((rules-list (racr-specification-rules-list spec))
            ; Support function, that given a rule R returns a list of all rules directly derivable from R:
            (derivable-rules
             (lambda (rule*)
               (fold-left
                (lambda (result symb*)
                  (if (symbol-non-terminal? symb*)
                      (append result (list (symbol-non-terminal? symb*)) (ast-rule-subtypes (symbol-non-terminal? symb*)))
                      result))
                (list)
                (cdr (ast-rule-production rule*))))))
       
       ;;; Resolve supertypes and non-terminals occuring in productions and ensure all non-terminals are defined:
       (for-each
        (lambda (rule*)
          (when (ast-rule-supertype? rule*)
            (let ((supertype-entry (racr-specification-find-rule spec (ast-rule-supertype? rule*))))
              (if (not supertype-entry)
                  (throw-exception
                   "Invalid AST rule " (ast-rule-as-symbol rule*) "; "
                   "The supertype " (ast-rule-supertype? rule*) " is not defined.")
                  (ast-rule-supertype?-set! rule* supertype-entry))))
          (for-each
           (lambda (symb*)
             (when (symbol-non-terminal? symb*)
               (let ((symb-definition (racr-specification-find-rule spec (symbol-name symb*))))
                 (when (not symb-definition)
                   (throw-exception
                    "Invalid AST rule " (ast-rule-as-symbol rule*) "; "
                    "Non-terminal " (symbol-name symb*) " is not defined."))
                 (symbol-non-terminal?-set! symb* symb-definition))))
           (cdr (ast-rule-production rule*))))
        rules-list)
       
       ;;; Ensure, that inheritance is cycle-free:
       (for-each
        (lambda (rule*)
          (when (memq rule* (ast-rule-subtypes rule*))
            (throw-exception
             "Invalid AST grammar; "
             "The definition of " (ast-rule-as-symbol rule*) " depends on itself (cyclic inheritance).")))
        rules-list)
       
       ;;; Ensure, that the start symbol is defined:
       (unless (racr-specification-find-rule spec start-symbol)
         (throw-exception
          "Invalid AST grammar; "
          "The start symbol " start-symbol " is not defined."))
       
       ;;; Resolve inherited production symbols:
       (apply-wrt-ast-inheritance
        (lambda (rule)
          (ast-rule-production-set!
           rule
           (append
            (list (car (ast-rule-production rule)))
            (map
             (lambda (symbol)
               (make-production-symbol
                (symbol-name symbol)
                rule
                (symbol-non-terminal? symbol)
                (symbol-kleene? symbol)
                (symbol-context-name symbol)
                (list)))
             (cdr (ast-rule-production (ast-rule-supertype? rule))))
            (cdr (ast-rule-production rule)))))
        rules-list)
       
       ;;; Ensure context-names are unique:
       (for-each
        (lambda (ast-rule)
          (for-each
           (lambda (symbol)
             (unless (eq? (ast-rule-find-child-context ast-rule (symbol-context-name symbol)) symbol)
               (throw-exception
                "Invalid AST grammar; "
                "The context name " (symbol-context-name symbol) " is not unique for rule " (ast-rule-as-symbol ast-rule) ".")))
           (cdr (ast-rule-production ast-rule))))
        rules-list)
       
       ;;; Ensure, that all non-terminals can be derived from the start symbol:
       (let* ((start-rule (racr-specification-find-rule spec start-symbol))
              (to-check (cons start-rule (ast-rule-subtypes start-rule)))
              (checked (list)))
         (let loop ()
           (unless (null? to-check)
             (let ((rule* (car to-check)))
               (set! to-check (cdr to-check))
               (set! checked (cons rule* checked))
               (for-each
                (lambda (derivable-rule)
                  (when (and
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
           (unless (null? non-derivable-rules)
             (throw-exception
              "Invalid AST grammar; "
              "The rules " (map ast-rule-as-symbol non-derivable-rules) " cannot be derived."))))
       
       ;;; Ensure, that all non-terminals are productive:
       (let* ((productive-rules (list))
              (to-check rules-list)
              (productive-rule?
               (lambda (rule*)
                 (not (find
                       (lambda (symb*)
                         (and
                          (symbol-non-terminal? symb*)
                          (not (symbol-kleene? symb*)) ; Unbounded repetitions are always productive because of the empty list.
                          (not (memq (symbol-non-terminal? symb*) productive-rules))))
                       (cdr (ast-rule-production rule*)))))))
         (let loop ()
           (let ((productive-rule
                  (find productive-rule? to-check)))
             (when productive-rule
               (set! to-check (remq productive-rule to-check))
               (set! productive-rules (cons productive-rule productive-rules))
               (loop))))
         (unless (null? to-check)
           (throw-exception
            "Invalid AST grammar; "
            "The rules " (map ast-rule-as-symbol to-check) " are not productive."))))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Attribute Specification ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (define-syntax specify-ag-rule
   (lambda (x)
     (syntax-case x ()
       ((_ spec att-name definition ...)
        (and (identifier? #'att-name) (not (null? #'(definition ...))))
        #'(let ((spec* spec)
                (att-name* 'att-name))
            (let-syntax
                ((specify-attribute*
                  (syntax-rules ()
                    ((_ spec* att-name* ((non-terminal index) equation))
                     (specify-attribute spec* att-name* 'non-terminal 'index #t equation #f))
                    ((_ spec* att-name* ((non-terminal index) cached? equation))
                     (specify-attribute spec* att-name* 'non-terminal 'index cached? equation #f))
                    ((_ spec* att-name* ((non-terminal index) equation bottom equivalence-function))
                     (specify-attribute spec* att-name* 'non-terminal 'index #t equation (cons bottom equivalence-function)))
                    ((_ spec* att-name* ((non-terminal index) cached? equation bottom equivalence-function))
                     (specify-attribute spec* att-name* 'non-terminal 'index cached? equation (cons bottom equivalence-function)))
                    ((_ spec* att-name* (non-terminal equation))
                     (specify-attribute spec* att-name* 'non-terminal 0 #t equation #f))
                    ((_ spec* att-name* (non-terminal cached? equation))
                     (specify-attribute spec* att-name* 'non-terminal 0 cached? equation #f))
                    ((_ spec* att-name* (non-terminal equation bottom equivalence-function))
                     (specify-attribute spec* att-name* 'non-terminal 0 #t equation (cons bottom equivalence-function)))
                    ((_ spec* att-name* (non-terminal cached? equation bottom equivalence-function))
                     (specify-attribute spec* att-name* 'non-terminal 0 cached? equation (cons bottom equivalence-function))))))
              (specify-attribute* spec* att-name* definition) ...))))))
 
 (define specify-attribute
   (lambda (spec attribute-name non-terminal context-name-or-position cached? equation circularity-definition)
     ;;; Before adding the attribute definition, ensure...
     (let ((wrong-argument-type ; ...correct argument types,...
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
       (when wrong-argument-type
         (throw-exception
          "Invalid attribute definition; "
          "Wrong argument type (" wrong-argument-type ").")))
     (unless (= (racr-specification-specification-phase spec) 2) ; ...that the RACR system is in the correct specification phase,...
       (throw-exception
        "Unexpected " attribute-name " attribute definition; "
        "Attributes can only be defined in the AG specification phase."))
     (let ((ast-rule (racr-specification-find-rule spec non-terminal)))
       (unless ast-rule ; ...the given AST rule is defined,...
         (throw-exception
          "Invalid attribute definition; "
          "The non-terminal " non-terminal " is not defined."))
       (let* ((context? ; ...the given context exists,...
               (if (symbol? context-name-or-position)
                   (if (eq? context-name-or-position '*)
                       (car (ast-rule-production ast-rule))
                       (ast-rule-find-child-context ast-rule context-name-or-position))
                   (if (>= context-name-or-position (length (ast-rule-production ast-rule)))
                       (throw-exception
                        "Invalid attribute definition; "
                        "There exists no " context-name-or-position "'th position in the context of " non-terminal ".")
                       (list-ref (ast-rule-production ast-rule) context-name-or-position)))))
         (unless context?
           (throw-exception
            "Invalid attribute definition; "
            "The non-terminal " non-terminal " has no " context-name-or-position " context."))
         (unless (symbol-non-terminal? context?) ; ...it is a non-terminal and...
           (throw-exception
            "Invalid attribute definition; "
            non-terminal context-name-or-position " is a terminal."))
         ; ...the attribute is not already defined for it:
         (when (memq attribute-name (map attribute-definition-name (symbol-attributes context?)))
           (throw-exception
            "Invalid attribute definition; "
            "Redefinition of " attribute-name " for " non-terminal context-name-or-position "."))
         ;;; Everything is fine. Thus, add the definition to the AST rule's respective symbol:
         (symbol-attributes-set!
          context?
          (cons
           (make-attribute-definition
            attribute-name
            context?
            equation
            circularity-definition
            cached?)
           (symbol-attributes context?)))))))
 
 (define compile-ag-specifications
   (lambda (spec)
     ;;; Ensure, that the RACR system is in the correct specification phase and...
     (let ((current-phase (racr-specification-specification-phase spec)))
       (when (< current-phase 2)
         (throw-exception
          "Unexpected AG compilation; "
          "The AST specifications are not yet compiled."))
       (if (> current-phase 2)
           (throw-exception
            "Unexpected AG compilation; "
            "The AG specifications already have been compiled.")
           (racr-specification-specification-phase-set! spec (+ current-phase 1)))) ; ...if so proceed to the next specification phase.
     
     ;;; Resolve attribute definitions inherited from a supertype. Thus,...
     (apply-wrt-ast-inheritance ; ...for every AST rule R which has a supertype...
      (lambda (rule)
        (let loop ((super-prod (ast-rule-production (ast-rule-supertype? rule)))
                   (sub-prod (ast-rule-production rule)))
          (unless (null? super-prod)
            (for-each ; ...check for every attribute definition of R's supertype...
             (lambda (super-att-def)
               (unless (find ; ...if it is shadowed by an attribute definition of R....
                        (lambda (sub-att-def)
                          (eq? (attribute-definition-name sub-att-def) (attribute-definition-name super-att-def)))
                        (symbol-attributes (car sub-prod)))
                 (symbol-attributes-set! ; ...If not, add...
                  (car sub-prod)
                  (cons
                   (make-attribute-definition ; ...a copy of the attribute definition inherited...
                    (attribute-definition-name super-att-def)
                    (car sub-prod) ; ...to R.
                    (attribute-definition-equation super-att-def)
                    (attribute-definition-circularity-definition super-att-def)
                    (attribute-definition-cached? super-att-def))
                   (symbol-attributes (car sub-prod))))))
             (symbol-attributes (car super-prod)))
            (loop (cdr super-prod) (cdr sub-prod)))))
      (racr-specification-rules-list spec))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Attribute Evaluation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (define (att-in-evaluation? n)
   (evaluator-state-in-evaluation? (node-evaluator-state n)))
 
 ; INTERNAL FUNCTION: Given a node n find a certain attribute associated with it, whereas in case no proper
 ; attribute is associated with n itself the search is extended to find a broadcast solution. If the
 ; extended search finds a solution, appropriate copy propergation attributes (i.e., broadcasters) are added.
 ; If no attribute instance can be found or n is a bud node, an exception is thrown. Otherwise, the
 ; attribute or its respective last broadcaster is returned.
 (define lookup-attribute
   (lambda (name n)
     (when (node-bud-node? n)
       (throw-exception
        "AG evaluator exception; "
        "Cannot access " name " attribute - the given node is a bud."))
     (let loop ((n n)) ; Recursively...
       (let ((att (node-find-attribute n name))) ; ...check if the current node has a proper attribute instance....
         (if att
             att ; ...If it has, return the found defining attribute instance.
             (let ((parent (node-parent n))) ; ...If no defining attribute instance can be found...
               (if (not parent) ; ...check if there exists a parent node that may provide a definition....
                   (throw-exception ; ...If not, throw an exception,...
                    "AG evaluator exception; "
                    "Cannot access unknown " name " attribute.")
                   (let* ((att (loop parent)) ; ...otherwise proceed the search at the parent node. If it succeeds...
                          (broadcaster ; ...construct a broadcasting attribute instance...
                           (make-attribute-instance
                            (make-attribute-definition ; ...whose definition context depends...
                             name
                             (if (eq? (node-ast-rule parent) 'list-node) ; ...if the parent node is a list node or not....
                                 (list-ref ; ...If it is a list node the broadcaster's context is...
                                  (ast-rule-production (node-ast-rule (node-parent parent))) ; ...the list node's parent node and...
                                  (node-child-index? parent)) ; ...child position.
                                 (list-ref ; ...If the parent node is not a list node the broadcaster's context is...
                                  (ast-rule-production (node-ast-rule parent)) ; ...the parent node and...
                                  (node-child-index? n))) ; ...the current node's child position. Further,...
                             (lambda (n . args) ; ...the broadcaster's equation just calls the parent node's counterpart. Finally,...
                               (apply att-value name (ast-parent n) args))
                             (attribute-definition-circularity-definition (attribute-instance-definition att))
                             #f)
                            n)))
                     (node-attributes-set! n (cons broadcaster (node-attributes n))) ; ...add the constructed broadcaster and...
                     broadcaster)))))))) ; ...return it as the current node's look-up result.
 
 (define att-value
   (lambda (name n . args)
     (let*-values (; The evaluator state used and changed throughout evaluation:
                   ((evaluator-state) (values (node-evaluator-state n)))
                   ; The attribute instance to evaluate:
                   ((att) (values (lookup-attribute name n)))
                   ; The attribute's definition:
                   ((att-def) (values (attribute-instance-definition att)))
                   ; The attribute cache entries used for evaluation and dependency tracking:
                   ((evaluation-att-cache dependency-att-cache)
                    (if (attribute-definition-cached? att-def)
                        ; If the attribute instance is cached, no special action is required, except...
                        (let ((att-cache
                               (or
                                ; ...finding the attribute cache entry to use...
                                (hashtable-ref (attribute-instance-cache att) args #f)
                                ; ...or construct a respective one.
                                (let ((new-entry (make-attribute-cache-entry att args)))
                                  (hashtable-set! (attribute-instance-cache att) args new-entry)
                                  new-entry))))
                          (values att-cache att-cache))
                        ; If the attribute is not cached, special attention must be paid to avoid the permament storing
                        ; of fixpoint results and attribute arguments on the one hand but still retaining correct
                        ; evaluation which requires these information on the other hand. To do so we introduce two
                        ; different types of attribute cache entries:
                        ; (1) A parameter approximating entry for tracking dependencies and influences of the uncached
                        ;     attribute instance.
                        ; (2) A set of temporary cycle entries for correct cycle detection and fixpoint computation.
                        ; The "cycle-value" field of the parameter approximating entry is misused to store the hashtable
                        ; containing the temporary cycle entries and must be deleted when evaluation finished.
                        (let* ((dependency-att-cache
                                (or
                                 (hashtable-ref (attribute-instance-cache att) racr-nil #f)
                                 (let ((new-entry (make-attribute-cache-entry att racr-nil)))
                                   (hashtable-set! (attribute-instance-cache att) racr-nil new-entry)
                                   (attribute-cache-entry-cycle-value-set!
                                    new-entry
                                    (make-hashtable equal-hash equal? 1))
                                   new-entry)))
                               (evaluation-att-cache
                                (or
                                 (hashtable-ref (attribute-cache-entry-cycle-value dependency-att-cache) args #f)
                                 (let ((new-entry (make-attribute-cache-entry att args)))
                                   (hashtable-set!
                                    (attribute-cache-entry-cycle-value dependency-att-cache)
                                    args
                                    new-entry)
                                   new-entry))))
                          (values evaluation-att-cache dependency-att-cache))))
                   ; Support function that given an intermediate fixpoint result checks if it is different from the
                   ; current cycle value and updates the cycle value and evaluator state accordingly:
                   ((update-cycle-cache)
                    (values
                     (lambda (new-result)
                       (unless ((cdr (attribute-definition-circularity-definition att-def))
                                new-result
                                (attribute-cache-entry-cycle-value evaluation-att-cache))
                         (attribute-cache-entry-cycle-value-set! evaluation-att-cache new-result)
                         (evaluator-state-ag-cycle-change?-set! evaluator-state #t))))))
       ; Decide how to evaluate the attribute dependening on whether its value already is cached or its respective
       ; cache entry is circular, already in evaluation or starting point of a fix-point computation:
       (cond
         ; CASE (0): Attribute already evaluated for given arguments:
         ((not (eq? (attribute-cache-entry-value evaluation-att-cache) racr-nil))
          ; Maintaine attribute cache entry dependencies, i.e., if this entry is evaluated throughout the
          ; evaluation of another entry, the other entry depends on this one. Afterwards,...
          (add-dependency:cache->cache dependency-att-cache)
          (attribute-cache-entry-value evaluation-att-cache)) ; ...return the cached value.
         
         ; CASE (1): Circular attribute that is starting point of a fixpoint computation:
         ((and (attribute-definition-circular? att-def) (not (evaluator-state-ag-in-cycle? evaluator-state)))
          (dynamic-wind
           (lambda ()
             ; Maintaine attribute cache entry dependencies, i.e., if this entry is evaluated throughout the
             ; evaluation of another entry, the other depends on this one. Further this entry depends
             ; on any other entry that will be evaluated through its own evaluation. Further,..
             (add-dependency:cache->cache dependency-att-cache)
             (evaluator-state-evaluation-stack-set!
              evaluator-state
              (cons dependency-att-cache (evaluator-state-evaluation-stack evaluator-state)))
             ; ...mark, that the entry is in evaluation and...
             (attribute-cache-entry-entered?-set! evaluation-att-cache #t)
             ; ...update the evaluator's state that we are about to start a fix-point computation.
             (evaluator-state-ag-in-cycle?-set! evaluator-state #t))
           (lambda ()
             (let loop () ; Start fix-point computation. Thus, as long as...
               (evaluator-state-ag-cycle-change?-set! evaluator-state #f) ; ...an entry's value changes...
               (update-cycle-cache (apply (attribute-definition-equation att-def) n args)) ; ...evaluate this entry.
               (when (evaluator-state-ag-cycle-change? evaluator-state)
                 (loop)))
             (let ((result (attribute-cache-entry-cycle-value evaluation-att-cache)))
               ; When fixpoint computation finished update the caches of all circular entries evaluated. To do so,...
               (let loop ((att-cache
                           (if (attribute-definition-cached? att-def)
                               evaluation-att-cache
                               dependency-att-cache)))
                 (let ((att-def (attribute-instance-definition (attribute-cache-entry-context att-cache))))
                   (if (not (attribute-definition-circular? att-def))
                       ; ...ignore non-circular entries and just proceed with the entries they depend on (to
                       ; ensure all strongly connected components within a weakly connected one are updated)....
                       (for-each
                        loop
                        (attribute-cache-entry-cache-dependencies att-cache))
                       ; ...In case of circular entries...
                       (if (attribute-definition-cached? att-def) ; ...check if they have to be cached and...
                           (when (eq? (attribute-cache-entry-value att-cache) racr-nil) ; ...are not already processed....
                             ; ...If so cache them,...
                             (attribute-cache-entry-value-set!
                              att-cache
                              (attribute-cache-entry-cycle-value att-cache))
                             (attribute-cache-entry-cycle-value-set! ; ...reset their cycle values to the bottom value and...
                              att-cache
                              (car (attribute-definition-circularity-definition att-def)))
                             (for-each ; ...proceed with the entries they depend on.
                              loop
                              (attribute-cache-entry-cache-dependencies att-cache)))
                           ; ...If a circular entry is not cached, check if it already is processed....
                           (when (> (hashtable-size (attribute-cache-entry-cycle-value att-cache)) 0)
                             ; ...If not, delete its temporary cycle cache and...
                             (hashtable-clear! (attribute-cache-entry-cycle-value att-cache))
                             (for-each ; ...proceed with the entries it depends on.
                              loop
                              (attribute-cache-entry-cache-dependencies att-cache)))))))
               result))
           (lambda ()
             ; Mark that fixpoint computation finished,...
             (evaluator-state-ag-in-cycle?-set! evaluator-state #f)
             ; the evaluation of the attribute cache entry finished and...
             (attribute-cache-entry-entered?-set! evaluation-att-cache #f)
             ; ...pop the entry from the evaluation stack.
             (evaluator-state-evaluation-stack-set!
              evaluator-state
              (cdr (evaluator-state-evaluation-stack evaluator-state))))))
         
         ; CASE (2): Circular attribute already in evaluation for the given arguments:
         ((and (attribute-definition-circular? att-def) (attribute-cache-entry-entered? evaluation-att-cache))
          ; Maintaine attribute cache entry dependencies, i.e., if this entry is evaluated throughout the
          ; evaluation of another entry, the other entry depends on this one. Finally,...
          (add-dependency:cache->cache dependency-att-cache)
          ; ...the intermediate fixpoint result is the attribute cache entry's cycle value.
          (attribute-cache-entry-cycle-value evaluation-att-cache))
         
         ; CASE (3): Circular attribute not in evaluation and entered throughout a fixpoint computation:
         ((attribute-definition-circular? att-def)
          (dynamic-wind
           (lambda ()
             ; Maintaine attribute cache entry dependencies, i.e., if this entry is evaluated throughout the
             ; evaluation of another entry, the other depends on this one. Further this entry depends
             ; on any other entry that will be evaluated through its own evaluation. Further,..
             (add-dependency:cache->cache dependency-att-cache)
             (evaluator-state-evaluation-stack-set!
              evaluator-state
              (cons dependency-att-cache (evaluator-state-evaluation-stack evaluator-state)))
             ; ...mark, that the entry is in evaluation.
             (attribute-cache-entry-entered?-set! evaluation-att-cache #t))
           (lambda ()
             (let ((result (apply (attribute-definition-equation att-def) n args))) ; Evaluate the entry and...
               (update-cycle-cache result) ; ...update its cycle value.
               result))
           (lambda ()
             ; Mark that the evaluation of the attribute cache entry finished and...
             (attribute-cache-entry-entered?-set! evaluation-att-cache #f)
             ; ...pop it from the evaluation stack.
             (evaluator-state-evaluation-stack-set!
              evaluator-state
              (cdr (evaluator-state-evaluation-stack evaluator-state))))))
         
         ; CASE (4): Non-circular attribute already in evaluation, i.e., unexpected cycle:
         ((attribute-cache-entry-entered? evaluation-att-cache)
          ; Maintaine attribute cache entry dependencies, i.e., if this entry is evaluated throughout the
          ; evaluation of another entry, the other entry depends on this one. Then,...
          (add-dependency:cache->cache dependency-att-cache)
          (throw-exception ; ...thrown an exception because we encountered an unexpected dependency cycle.
           "AG evaluator exception; "
           "Unexpected " name " cycle."))
         
         (else ; CASE (5): Non-circular attribute not in evaluation:
          (dynamic-wind
           (lambda ()
             ; Maintaine attribute cache entry dependencies, i.e., if this entry is evaluated throughout the
             ; evaluation of another entry, the other depends on this one. Further this entry depends
             ; on any other entry that will be evaluated through its own evaluation. Further,...
             (add-dependency:cache->cache dependency-att-cache)
             (evaluator-state-evaluation-stack-set!
              evaluator-state
              (cons dependency-att-cache (evaluator-state-evaluation-stack evaluator-state)))
             ; ...mark, that the entry is in evaluation.
             (attribute-cache-entry-entered?-set! evaluation-att-cache #t))
           (lambda ()
             (let ((result (apply (attribute-definition-equation att-def) n args))) ; Evaluate the entry and,...
               (when (attribute-definition-cached? att-def) ; ...if caching is enabled,...
                 (attribute-cache-entry-value-set! evaluation-att-cache result)) ; ...cache its value.
               result))
           (lambda ()
             ; Mark that the evaluation of the attribute cache entry finished and...
             (if (attribute-definition-cached? att-def)
                 (attribute-cache-entry-entered?-set! evaluation-att-cache #f)
                 (hashtable-delete! (attribute-cache-entry-cycle-value dependency-att-cache) args))
             ; ...pop it from the evaluation stack.
             (evaluator-state-evaluation-stack-set!
              evaluator-state
              (cdr (evaluator-state-evaluation-stack evaluator-state))))))))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Specification Queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; General Note: Because RACR specifications never change after compilation, there is no need to add and
 ;   maintain dependencies when attributes query specifications. The specification query API therefore just
 ;   forwards to the respective internal functions. Lists must be copied before they are returned however.
 
 ; Specification Queries:
 
 (define specification->phase
   (lambda (spec)
     (racr-specification-specification-phase spec)))
 
 (define specification->start-symbol
   (lambda (spec)
     (racr-specification-start-symbol spec)))
 
 (define specification->ast-rules
   (lambda (spec)
     (racr-specification-rules-list spec))) ; Already creates copy!
 
 (define specification->find-ast-rule
   (lambda (spec node-type)
     (racr-specification-find-rule spec node-type)))
 
 ; AST Rule Queries:
 
 (define ast-rule->symbolic-representation
   (lambda (ast-rule)
     (ast-rule-as-symbol ast-rule)))
 
 (define ast-rule->supertype?
   (lambda (ast-rule)
     (ast-rule-supertype? ast-rule)))
 
 (define ast-rule->production
   (lambda (rule)
     (append (ast-rule-production rule) (list)))) ; Create copy!
 
 ; Production Symbol Queries:
 
 (define symbol->name
   (lambda (symb)
     (symbol-name symb)))
 
 (define symbol->non-terminal?
   (lambda (symb)
     (symbol-non-terminal? symb)))
 
 (define symbol->kleene?
   (lambda (symb)
     (symbol-kleene? symb)))
 
 (define symbol->context-name
   (lambda (symb)
     (symbol-context-name symb)))
 
 (define symbol->attributes
   (lambda (symbol)
     (append (symbol-attributes symbol) (list)))) ; Create copy!
 
 ; Attribute Definition Queries:
 
 (define attribute->name
   (lambda (att-def)
     (attribute-definition-name att-def)))
 
 (define attribute->circular?
   (lambda (att-def)
     (attribute-definition-circular? att-def)))
 
 (define attribute->synthesized?
   (lambda (att-def)
     (attribute-definition-synthesized? att-def)))
 
 (define attribute->inherited?
   (lambda (att-def)
     (attribute-definition-inherited? att-def)))
 
 (define attribute->cached?
   (lambda (att-def)
     (attribute-definition-cached? att-def)))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Abstract Syntax Tree Queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (define ast-node? ; Scheme entities are either allocated as AST nodes or never will be => No need to add dependencies!
   (lambda (n)
     (node? n)))
 
 (define ast-specification
   (lambda (n)
     (when (or (ast-list-node? n) (ast-bud-node? n)) ; Remember: Terminal nodes as such are never exposed to users.
       (throw-exception
        "Cannot query specification; "
        "List and bud nodes are not part of any specification."))
     ; The specification of a node can never change => No need to add dependencies!
     (ast-rule-specification (node-ast-rule n))))
 
 (define ast-list-node? ; No dependency tracking needed!
   (lambda (n)
     (node-list-node? n)))
 
 (define ast-bud-node? ; No dependency tracking needed!
   (lambda (n)
     (node-bud-node? n)))
 
 (define ast-node-rule
   (lambda (n)
     (when (or (ast-list-node? n) (ast-bud-node? n)) ; Remember: Terminal nodes as such are never exposed to users.
       (throw-exception
        "Cannot query type; "
        "List and bud nodes have no type."))
     (add-dependency:cache->node-type n)
     (node-ast-rule n)))
 
 (define ast-node-type
   (lambda (n)
     (symbol-name (car (ast-rule-production (ast-node-rule n))))))
 
 (define ast-subtype?
   (lambda (a1 a2)
     (when (or
            (and (ast-node? a1) (or (ast-list-node? a1) (ast-bud-node? a1)))
            (and (ast-node? a2) (or (ast-list-node? a2) (ast-bud-node? a2))))
       (throw-exception
        "Cannot perform subtype check; "
        "List and bud nodes cannot be tested for subtyping."))
     (when (and (not (ast-node? a1)) (not (ast-node? a2)))
       (throw-exception
        "Cannot perform subtype check; "
        "At least one argument must be an AST node."))
     ((lambda (t1/t2)
        (and
         (car t1/t2)
         (cdr t1/t2)
         (ast-rule-subtype? (car t1/t2) (cdr t1/t2))))
      (if (symbol? a1)
          (let* ((t2 (node-ast-rule a2))
                 (t1 (racr-specification-find-rule (ast-rule-specification t2) a1)))
            (unless t1
              (throw-exception
               "Cannot perform subtype check; "
               a1 " is no valid non-terminal (first argument undefined non-terminal)."))
            (add-dependency:cache->node-super-type a2 t1)
            (cons t1 t2))
          (if (symbol? a2)
              (let* ((t1 (node-ast-rule a1))
                     (t2 (racr-specification-find-rule (ast-rule-specification t1) a2)))
                (unless t1
                  (throw-exception
                   "Cannot perform subtype check; "
                   a2 " is no valid non-terminal (second argument undefined non-terminal)."))
                (add-dependency:cache->node-sub-type a1 t2)
                (cons t1 t2))
              (begin
                (add-dependency:cache->node-sub-type a1 (node-ast-rule a2))
                (add-dependency:cache->node-super-type a2 (node-ast-rule a1))
                (cons (node-ast-rule a1) (node-ast-rule a2))))))))
 
 (define ast-has-parent?
   (lambda (n)
     (let ((parent (node-parent n)))
       (if parent
           (begin
             (add-dependency:cache->node-upwards parent)
             parent)
           (begin
             (add-dependency:cache->node-is-root n)
             #f)))))
 
 (define ast-parent
   (lambda (n)
     (let ((parent (node-parent n)))
       (unless parent
         (throw-exception "Cannot query parent of roots."))
       (add-dependency:cache->node-upwards parent)
       parent)))
 
 (define ast-has-child?
   (lambda (context-name n)
     (add-dependency:cache->node-defines-context n context-name)
     (if (node-find-child n context-name) #t #f))) ; BEWARE: Never return the child if it exists, but instead just #t!
 
 (define ast-child
   (lambda (i n)
     (let ((child
            (if (symbol? i)
                (node-find-child n i)
                (and (>= i 1) (<= i (length (node-children n))) (list-ref (node-children n) (- i 1))))))
       (unless child
         (throw-exception "Cannot query non-existent " i (if (symbol? i) "" "'th") " child."))
       (add-dependency:cache->node-downwards child)
       (if (node-terminal? child)
           (node-children child)
           child))))
 
 (define ast-has-sibling?
   (lambda (context-name n)
     (let ((parent? (ast-has-parent? n)))
       (and parent? (ast-has-child? context-name parent?)))))
 
 (define ast-sibling
   (lambda (i n)
     (ast-child i (ast-parent n))))
 
 (define ast-child-index
   (lambda (n)
     (ast-find-child*
      (lambda (i child)
        (if (eq? child n) i #f))
      (ast-parent n))))
 
 (define ast-num-children
   (lambda (n)
     (add-dependency:cache->node-num-children n)
     (length (node-children n))))
 
 (define ast-children
   (lambda (n . b)
     (reverse
      (let ((result (list)))
        (apply
         ast-for-each-child
         (lambda (i child)
           (set! result (cons child result)))
         n
         b)
        result))))
 
 (define ast-for-each-child
   (lambda (f n . b)
     (let ((b (if (null? b) (list (cons 1 '*)) b)))
       (for-each
        (lambda (b)
          (if (eq? (cdr b) '*)
              (let ((pos (car b))
                    (ub (length (node-children n))))
                (dynamic-wind
                 (lambda () #f)
                 (lambda ()
                   (let loop ()
                     (when (<= pos ub)
                       (f pos (ast-child pos n))
                       (set! pos (+ pos 1))
                       (loop))))
                 (lambda ()
                   (when (> pos ub)
                     (ast-num-children n))))) ; BEWARE: Access to number of children ensures proper dependency tracking!
              (let loop ((pos (car b)))
                (when (<= pos (cdr b))
                  (f pos (ast-child pos n))
                  (loop (+ pos 1))))))
        b))))
 
 (define ast-find-child
   (lambda (f n . b)
     (call/cc
      (lambda (c)
        (apply
         ast-for-each-child
         (lambda (i child)
           (when (f i child)
             (c child)))
         n
         b)
        #f))))
 
 (define ast-find-child*
   (lambda (f n . b)
     (call/cc
      (lambda (c)
        (apply
         ast-for-each-child
         (lambda (i child)
           (let ((res (f i child)))
             (when res
               (c res))))
         n
         b)
        #f))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Abstract Syntax Tree Construction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (define (create-ast-2 spec rule children)
   (apply (=ast-node-factory (racr-specification-2-ast-scheme spec) rule) children))
 
 (define (create-ast-list-2 children)
   ;;; Before constructing the list node ensure, that...
   (unless (for-all can-be-list-element? children) ; ...all children fit.
     (throw-exception
      "Cannot construct list node;"
      "The given children do not fit."))
   ;;; When all constraints are satisfied,...
   (for-each ; ...flush all attribute cache entries depending on the children being roots,...
    (lambda (child)
      (for-each
       (lambda (influence)
         (flush-attribute-cache-entry (car influence)))
       (filter
        (lambda (influence)
          (vector-ref (cdr influence) 1))
        (node-cache-influences child))))
    children)
   (let ((new-list (make-node 'list-node #f (append children (list))))) ; ...construct the list node,...
     (for-each ; ...set it as parent of each child,...
      (lambda (child)
        (node-parent-set! child new-list))
      children)
     (distribute-evaluator-state (make-evaluator-state) new-list) ; ...distribute its evaluator state and...
     new-list)) ; ...return it.
 
 ; INTERNAL FUNCTION: Is a Scheme entity permitted as non-terminal child of other AST nodes?
 (define (can-be-non-terminal-child? node)
   (and
    (ast-node? node) ; The node is a non-terminal node (Remember: Terminal nodes as such are never exposed to users),...
    (not (node-parent node)) ; ...not already part of another AST and...
    (not (evaluator-state-in-evaluation? (node-evaluator-state node))))) ; ...non of its attributes are in evaluation.
 
 ; INTERNAL FUNCTION: Is a Scheme entity permitted as element in list nodes? 
 (define (can-be-list-element? node)
   (and
    (can-be-non-terminal-child? node) ; The node can be a non-terminal child and...
    (not (node-list-node? node)))) ; ...is not a list node.
 
 ; INTERNAL FUNCTION: Given a node in some context update its attribution (the given context must be valid).
 (define (update-attributes-2 parent-type context-name n)
   (define (equal-semantics a1 a2) ; Evaluate two attribute definitions always the same?
     (or ; Attribute definitions are semantically equivalent, if either...
      (eq? a1 a2) ; ...they are the same or...
      (and
       (eq? (->equation a1) (->equation a2)) ; ...have the same equation and...
       (if (=circular? a1) ; ...circularity definition.
           (and
            (=circular? a2)
            (equal? (=bottom-value a1) (=bottom-value a2))
            (eq? (=equality-function a1) (=equality-function a2)))
           (not (=circular? a2))))))
   ;;; Update the node's attributes w.r.t. its context. To do so,...
   (if (node-list-node? n) ; ...check if the node is a list. If so,...
       (for-each (lambda (n) (update-attributes-2 parent-type context-name n)) (node-children n)) ; ...update all its elements...
       (unless (or (node-terminal? n) (node-bud-node? n)) ; ...otherwise ensure it is an ordinary non-terminal. If so,...
         (let ((old-attribute-instances (node-attributes n)))
           (node-attributes-set! ; ...construct the list of its attribute instances, i.e.,...
            n
            (map ; ...for each...
             (lambda (attribute)
               (let ((instance? (node-find-attribute-2 n (->name attribute)))) ; ...existing instance with...
                 (if (and instance? (equal-semantics (attribute-instance-definition instance?) attribute)) ; ...unchaged semantics...
                     (begin
                       (attribute-instance-definition-set! instance? attribute) ; ...just update the instance's definition...
                       instance?)
                     (make-attribute-instance attribute n)))) ; ...If a proper instance does not yet exist, add it. Finally,...
             (=attributes-for-context
              (node-ast-rule n)
              parent-type
              context-name
              (->name (node-ast-rule n)))))
           (for-each ; ...flush the cache of all instances no more defined or whose definition changed.
            (lambda (attribute-instance)
              (when (not (memq attribute-instance (node-attributes n)))
                (flush-attribute-instance attribute-instance)
                (attribute-instance-context-set! attribute-instance racr-nil)))
            old-attribute-instances)))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (define create-ast
   (lambda (spec rule children)
     ;;; Before constructing the node ensure, that...
     (when (< (racr-specification-specification-phase spec) 3) ; ...the RACR system is completely specified,...
       (throw-exception
        "Cannot construct " rule " fragment; "
        "The RACR specification still must be compiled."))
     (let* ((ast-rule (racr-specification-find-rule spec rule))
            (new-fragment
             (make-node
              ast-rule
              #f
              (list))))
       (unless ast-rule ; ...the given AST rule is defined,...
         (throw-exception
          "Cannot construct " rule " fragment; "
          "Unknown non-terminal/rule."))
       (unless (satisfies-contexts? children (cdr (ast-rule-production ast-rule))) ; ...and the children fit.
         (throw-exception
          "Cannot construct " rule " fragment; "
          "The given children do not fit."))
       ;;; When all constraints are satisfied, construct the new fragment,...
       (node-children-set! ; ...add its children,...
        new-fragment
        (map ; ...set it as parent of each child,...
         (lambda (symbol child)
           (if (symbol-non-terminal? symbol)
               (begin
                 (for-each ; ...flush all attribute cache entries depending on any added child being a root,...
                  (lambda (influence)
                    (flush-attribute-cache-entry (car influence)))
                  (filter
                   (lambda (influence)
                     (vector-ref (cdr influence) 1))
                   (node-cache-influences child)))
                 (node-parent-set! child new-fragment)
                 child)
               (make-node 'terminal new-fragment child)))
         (cdr (ast-rule-production ast-rule))
         children))
       (distribute-evaluator-state (make-evaluator-state) new-fragment) ; ...distribute the new fragment's evaluator state and...
       (update-synthesized-attribution new-fragment) ; ...initialize its synthesized and...
       (for-each ; ...each child's inherited attributes.
        update-inherited-attribution
        (node-children new-fragment))
       new-fragment))) ; Finally, return the newly constructed fragment.
 
 (define create-ast-list
   (lambda (children)
     ;;; Before constructing the list node ensure, that...
     (let ((new-list
            (make-node
             'list-node
             #f
             (append children (list))))) ; BEWARE: create copy of children!
       (unless
           (for-all ; ...all children fit.
               (lambda (child)
                 (valid-list-element-candidate? new-list child))
             children)
         (throw-exception
          "Cannot construct list node; "
          "The given children do not fit."))
       ;;; When all constraints are satisfied,...
       (for-each ; ...flush all attribute cache entries depending on the children being roots,...
        (lambda (child)
          (for-each
           (lambda (influence)
             (flush-attribute-cache-entry (car influence)))
           (filter
            (lambda (influence)
              (vector-ref (cdr influence) 1))
            (node-cache-influences child))))
        children)
       (for-each ; ...set the new list node as parent of every child,...
        (lambda (child)
          (node-parent-set! child new-list))
        children)
       (distribute-evaluator-state (make-evaluator-state) new-list) ; ...construct and distribute its evaluator state and...
       new-list))) ; ...return it.
 
 (define create-ast-bud
   (lambda ()
     (let ((bud-node (make-node 'bud-node #f (list))))
       (distribute-evaluator-state (make-evaluator-state) bud-node)
       bud-node)))
 
 (define create-ast-mockup
   (lambda (rule)
     (create-ast
      (ast-rule-specification rule)
      (symbol-name (car (ast-rule-production rule)))
      (map
       (lambda (symbol)
         (cond
           ((not (symbol-non-terminal? symbol))
            racr-nil)
           ((symbol-kleene? symbol)
            (create-ast-list (list)))
           (else (create-ast-bud))))
       (cdr (ast-rule-production rule))))))
 
 ; INTERNAL FUNCTION: Given two non-terminal nodes, return if the second can replace the first regarding its context.
 (define valid-replacement-candidate?
   (lambda (node candidate)
     (if (node-list-node? (node-parent node))
         (valid-list-element-candidate? (node-parent node) candidate)
         (and
          (satisfies-context?
           candidate
           (list-ref (ast-rule-production (node-ast-rule (node-parent node))) (node-child-index? node)))
          (not (node-inside-of? node candidate))))))
 
 ; INTERNAL FUNCTION: Given a list node and another node, return if the other node can become element of
 ; the list node regarding its context.
 (define valid-list-element-candidate?
   (lambda (list-node candidate)
     (let ((expected-type? ; If the list node has a parent, its parent induces a type for the list's elements.
            (if (node-parent list-node)
                (symbol-non-terminal?
                 (list-ref
                  (ast-rule-production (node-ast-rule (node-parent list-node)))
                  (node-child-index? list-node)))
                #f)))
       (and ; The given candidate can be element of the list, if (1)...
        (if expected-type? ; ...either,...
            (satisfies-context? candidate expected-type? #f) ; ...the candidate fits regarding the context in which the list is, or,...
            (and ; ...in case no type is induced for the list's elements,...
             (ast-node? candidate) ; ...the candiate is a non-terminal node,...
             (not (node-list-node? candidate)) ; ...not a list node,...
             (not (node-parent candidate)) ; ...not already part of another AST and...
             (not (evaluator-state-in-evaluation? (node-evaluator-state candidate))))) ; ...non of its attributes are in evaluation,...
        (not (node-inside-of? list-node candidate)))))) ; ...and (2) its spaned AST does not contain the list node.
 
 ; INTERNAL FUNCTION: Given a node or terminal value and a context, return if the
 ; node/terminal value can become a child of the given context.
 (define satisfies-context?
   (case-lambda
     ((child context)
      (satisfies-context? child (symbol-non-terminal? context) (symbol-kleene? context)))
     ((child non-terminal? kleene?)
      (or ; The given child is valid if either,...
       (not non-terminal?) ; ...a terminal is expected or,...
       (and ; ...in case a non-terminal is expected,...
        (ast-node? child) ; ...the given child is an AST node,...
        (not (node-parent child)) ; ...does not already belong to another AST,...
        (not (evaluator-state-in-evaluation? (node-evaluator-state child))) ; ...non of its attributes are in evaluation and...
        (or
         (node-bud-node? child) ; ...the child either is a bud node or,...
         (if kleene?
             (and ; ...in case a list node is expected,...
              (node-list-node? child) ; ...is a list...
              (for-all ; ...whose children are...
                  (lambda (child)
                    (or ; ...either bud nodes or nodes of the expected type, or,...
                     (node-bud-node? child)
                     (ast-rule-subtype? (node-ast-rule child) non-terminal?)))
                (node-children child)))
             (and ; ...in case a non-list node is expected,...
              (not (node-list-node? child)) ; ...is a non-list node of...
              (ast-rule-subtype? (node-ast-rule child) non-terminal?))))))))) ; ...the expected type.
 
 ; INTERNAL FUNCTION: Given list of nodes or terminal values and a list of contexts, return if the
 ; nodes/terminal values can become children of the given contexts.
 (define satisfies-contexts?
   (lambda (children contexts)
     (and
      (= (length children) (length contexts))
      (for-all satisfies-context? children contexts))))
 
 ; INTERNAL FUNCTION: Given an AST node update its synthesized attribution, i.e., add missing synthesized
 ; attributes, delete superfluous ones, shadow equally named inherited attributes and update the
 ; definitions of existing synthesized attributes.
 (define update-synthesized-attribution
   (lambda (n)
     (when (and (not (node-terminal? n)) (not (node-list-node? n)) (not (node-bud-node? n)))
       (for-each
        (lambda (att-def)
          (let ((att (node-find-attribute n (attribute-definition-name att-def))))
            (cond
              ((not att)
               (node-attributes-set! n (cons (make-attribute-instance att-def n) (node-attributes n))))
              ((eq? (attribute-definition-equation (attribute-instance-definition att)) (attribute-definition-equation att-def))
               (attribute-instance-definition-set! att att-def))
              (else
               (flush-attribute-instance att)
               (node-attributes-set!
                n
                (cons (make-attribute-instance att-def n) (remq att (node-attributes n))))))))
        (symbol-attributes (car (ast-rule-production (node-ast-rule n)))))
       (node-attributes-set! ; Delete all synthesized attribute instances not defined anymore:
        n
        (remp
         (lambda (att)
           (let ((remove?
                  (and
                   (attribute-definition-synthesized? (attribute-instance-definition att))
                   (not
                    (eq?
                     (symbol-ast-rule (attribute-definition-context (attribute-instance-definition att)))
                     (node-ast-rule n))))))
             (when remove?
               (flush-attribute-instance att))
             remove?))
         (node-attributes n))))))
 
 ; INTERNAL FUNCTION: Given an AST node update its inherited attribution, i.e., add missing inherited
 ; attributes, delete superfluous ones and update the definitions of existing inherited attributes.
 ; If the given node is a list-node the inherited attributes of its elements are updated.
 (define update-inherited-attribution
   (lambda (n)
     ;;; Support function updating n's inherited attribution w.r.t. a list of inherited attribute definitions:
     (define update-by-defs
       (lambda (n att-defs)
         (for-each ; Add new and update existing inherited attribute instances:
          (lambda (att-def)
            (let ((att (node-find-attribute n (attribute-definition-name att-def))))
              (cond
                ((not att)
                 (node-attributes-set! n (cons (make-attribute-instance att-def n) (node-attributes n))))
                ((not (attribute-definition-synthesized? (attribute-instance-definition att)))
                 (if (eq?
                      (attribute-definition-equation (attribute-instance-definition att))
                      (attribute-definition-equation att-def))
                     (attribute-instance-definition-set! att att-def)
                     (begin
                       (flush-attribute-instance att)
                       (node-attributes-set!
                        n
                        (cons (make-attribute-instance att-def n) (remq att (node-attributes n))))))))))
          att-defs)
         (node-attributes-set! ; Delete all inherited attribute instances not defined anymore:
          n
          (remp
           (lambda (att)
             (let ((remove?
                    (and
                     (attribute-definition-inherited? (attribute-instance-definition att))
                     (not (memq (attribute-instance-definition att) att-defs)))))
               (when remove?
                 (flush-attribute-instance att))
               remove?))
           (node-attributes n)))))
     ;;; Perform the update:
     (let* ((parent (node-parent n))
            (att-defs
             (cond
               ((not parent)
                (list))
               ((not (node-list-node? parent))
                (symbol-attributes
                 (list-ref
                  (ast-rule-production (node-ast-rule parent))
                  (node-child-index? n))))
               ((node-parent parent)
                (symbol-attributes
                 (list-ref
                  (ast-rule-production (node-ast-rule (node-parent parent)))
                  (node-child-index? parent))))
               (else (list)))))
       (if (node-list-node? n)
           (for-each
            (lambda (n)
              (unless (node-bud-node? n)
                (update-by-defs n att-defs)))
            (node-children n))
           (unless (node-bud-node? n)
             (update-by-defs n att-defs))))))
 
 ; INTERNAL FUNCTION: Given an AST node delete its inherited attribute instances. Iff the given node
 ; is a list node, the inherited attributes of its elements are deleted.
 (define detach-inherited-attributes
   (lambda (n)
     (cond
       ((node-list-node? n)
        (for-each
         detach-inherited-attributes
         (node-children n)))
       ((node-non-terminal? n)
        (node-attributes-set!
         n
         (remp
          (lambda (att)
            (let ((remove? (attribute-definition-inherited? (attribute-instance-definition att))))
              (when remove?
                (flush-attribute-instance att))
              remove?))
          (node-attributes n)))))))
 
 ; INTERNAL FUNCTION: Given an evaluator state and an AST fragment, change the
 ; fragment's evaluator state to the given one.
 (define distribute-evaluator-state
   (lambda (evaluator-state n)
     (node-evaluator-state-set! n evaluator-state)
     (unless (node-terminal? n)
       (for-each
        (lambda (n)
          (distribute-evaluator-state evaluator-state n))
        (node-children n)))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dependency Tracking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
 (define add-dependency:cache->node-upwards
   (lambda (influencing-node)
     (add-dependency:cache->node-characteristic influencing-node (cons 0 'up))))

 ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
 (define add-dependency:cache->node-downwards
   (lambda (influencing-node)
     (add-dependency:cache->node-characteristic influencing-node (cons 0 'down))))
 
 ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
 (define add-dependency:cache->node-is-root
   (lambda (influencing-node)
     (add-dependency:cache->node-characteristic influencing-node (cons 1 racr-nil))))
 
 ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
 (define add-dependency:cache->node-num-children
   (lambda (influencing-node)
     (add-dependency:cache->node-characteristic influencing-node (cons 2 racr-nil))))
 
 ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
 (define add-dependency:cache->node-type
   (lambda (influencing-node)
     (add-dependency:cache->node-characteristic influencing-node (cons 3 racr-nil))))
 
 ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
 (define add-dependency:cache->node-super-type
   (lambda (influencing-node comparision-type)
     (add-dependency:cache->node-characteristic influencing-node (cons 4 comparision-type))))
 
 ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
 (define add-dependency:cache->node-sub-type
   (lambda (influencing-node comparision-type)
     (add-dependency:cache->node-characteristic influencing-node (cons 5 comparision-type))))
 
 ; INTERNAL FUNCTION: See "add-dependency:cache->node-characteristic".
 (define add-dependency:cache->node-defines-context
   (lambda (influencing-node context-name)
     (add-dependency:cache->node-characteristic influencing-node (cons 6 context-name))))
 
 ; INTERNAL FUNCTION: Given a node N and a correlation C add an dependency-edge marked with C from
 ; the attribute cache entry currently in evaluation (considering the evaluator state of the AST N
 ; is part of) to N and an influence-edge vice versa. If no attribute cache entry is in evaluation
 ; no edges are added. The following seven correlations exist:
 ;  (0) Dependency on the existence of the node w.r.t. a query from a certain direction encoded in C (i.e.,
 ;      existence of a node at the same location queried from the same direction (upwards or downwards the AST))
 ;  (1) Dependency on the node being a root (i.e., the node has no parent)
 ;  (2) Dependency on the node's number of children (i.e., existence of a node at the same location and with
 ;      the same number of children)
 ;  (3) Dependency on the node's type (i.e., existence of a node at the same location and with the same type)
 ;  (4) Dependency on whether the node's type is a supertype w.r.t. a certain type encoded in C or not
 ;  (5) Dependency on whether the node's type is a subtype w.r.t. a certain type encoded in C or not
 ;  (6) Dependency on whether the node defines a certain context (i.e., has child with a certain name) or not
 (define add-dependency:cache->node-characteristic
   (lambda (influencing-node correlation)
     (let ((dependent-cache (evaluator-state-in-evaluation? (node-evaluator-state influencing-node))))
       (when dependent-cache
         (let ((dependency-vector
                (let ((dc-hit (assq influencing-node (attribute-cache-entry-node-dependencies dependent-cache))))
                  (and dc-hit (cdr dc-hit)))))
           (unless dependency-vector
             (set! dependency-vector (vector #f #f #f #f (list) (list) (list)))
             (attribute-cache-entry-node-dependencies-set!
              dependent-cache
              (cons
               (cons influencing-node dependency-vector)
               (attribute-cache-entry-node-dependencies dependent-cache)))
             (node-cache-influences-set!
              influencing-node
              (cons
               (cons dependent-cache dependency-vector)
               (node-cache-influences influencing-node))))
           (let ((correlation-type (car correlation))
                 (correlation-arg (cdr correlation)))
             (vector-set!
              dependency-vector
              correlation-type
              (case correlation-type
                ((0)
                 (let ((known-direction (vector-ref dependency-vector correlation-type)))
                   (cond
                     ((not known-direction) correlation-arg)
                     ((eq? known-direction correlation-arg) known-direction)
                     (else 'up/down))))
                ((1 2 3)
                 #t)
                ((4 5 6)
                 (let ((known-args (vector-ref dependency-vector correlation-type)))
                   (if (memq correlation-arg known-args)
                       known-args
                       (cons correlation-arg known-args))))))))))))
 
 ; INTERNAL FUNCTION: Given an attribute cache entry C, add an dependency-edge from C to the entry currently
 ; in evaluation (considering the evaluator state of the AST C is part of) and an influence-edge vice-versa.
 ; If no attribute cache entry is in evaluation no edges are added.
 (define add-dependency:cache->cache
   (lambda (influencing-cache)
     (let ((dependent-cache
            (evaluator-state-in-evaluation?
             (node-evaluator-state
              (attribute-instance-context
               (attribute-cache-entry-context influencing-cache))))))
       (when (and dependent-cache (not (memq influencing-cache (attribute-cache-entry-cache-dependencies dependent-cache))))
         (attribute-cache-entry-cache-dependencies-set!
          dependent-cache
          (cons
           influencing-cache
           (attribute-cache-entry-cache-dependencies dependent-cache)))
         (attribute-cache-entry-cache-influences-set!
          influencing-cache
          (cons
           dependent-cache
           (attribute-cache-entry-cache-influences influencing-cache)))))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Primitive Rewrites ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ; INTERNAL FUNCTION: Given an attribute instance, flush all its cache entries.
 (define flush-attribute-instance
   (lambda (att)
     (call-with-values
      (lambda ()
        (hashtable-entries (attribute-instance-cache att)))
      (lambda (keys values)
        (vector-for-each
         flush-attribute-cache-entry
         values)))))
 
 ; INTERNAL FUNCTION: Given an attribute cache entry, delete it and all depending entries.
 (define flush-attribute-cache-entry
   (lambda (att-cache)
     (let ((influenced-caches (attribute-cache-entry-cache-influences att-cache))) ; Save all influenced attribute cache entries.
       ; Delete foreign influences:
       (for-each ; For every cache entry I the entry depends on,...
        (lambda (influencing-cache)
          (attribute-cache-entry-cache-influences-set! ; ...remove the influence edge from I to the entry.
           influencing-cache
           (remq att-cache (attribute-cache-entry-cache-influences influencing-cache))))
        (attribute-cache-entry-cache-dependencies att-cache))
       (for-each ; For every node N the attribute cache entry depends on...
        (lambda (node-dependency)
          (node-cache-influences-set!
           (car node-dependency)
           (remp ; ...remove the influence edge from N to the entry.
            (lambda (cache-influence)
              (eq? (car cache-influence) att-cache))
            (node-cache-influences (car node-dependency)))))
        (attribute-cache-entry-node-dependencies att-cache))
       ; Delete the attribute cache entry:
       (hashtable-delete!
        (attribute-instance-cache (attribute-cache-entry-context att-cache))
        (attribute-cache-entry-arguments att-cache))
       (attribute-cache-entry-cache-dependencies-set! att-cache (list))
       (attribute-cache-entry-node-dependencies-set! att-cache (list))
       (attribute-cache-entry-cache-influences-set! att-cache (list))
       ; Proceed flushing, i.e., for every attribute cache entry D the entry originally influenced,...
       (for-each
        (lambda (dependent-cache)
          (flush-attribute-cache-entry dependent-cache)) ; ...flush D.
        influenced-caches))))
 
 ; INTERNAL FUNCTION: Given an AST node n, flush all attribute cache entries that depend on
 ; information of the subtree spaned by n but are outside of it and, if requested, all attribute
 ; cache entries within the subtree spaned by n that depend on information outside of it.
 (define flush-inter-fragment-dependent-attribute-cache-entries
   (lambda (n flush-outgoing?)
     (let loop ((n* n))
       (for-each
        (lambda (influence)
          (unless (node-inside-of? (attribute-instance-context (attribute-cache-entry-context (car influence))) n)
            (flush-attribute-cache-entry (car influence))))
        (node-cache-influences n*))
       (for-each
        (lambda (att)
          (vector-for-each
           (lambda (att-cache)
             (let ((flush-att-cache?
                    (and
                     flush-outgoing?
                     (or
                      (find
                       (lambda (dependency)
                         (not (node-inside-of? (car dependency) n)))
                       (attribute-cache-entry-node-dependencies att-cache))
                      (find
                       (lambda (influencing-cache)
                         (not (node-inside-of? (attribute-instance-context (attribute-cache-entry-context influencing-cache)) n)))
                       (attribute-cache-entry-cache-dependencies att-cache))))))
               (if flush-att-cache?
                   (flush-attribute-cache-entry att-cache)
                   (for-each
                    (lambda (dependent-cache)
                      (unless (node-inside-of? (attribute-instance-context (attribute-cache-entry-context dependent-cache)) n)
                        (flush-attribute-cache-entry dependent-cache)))
                    (attribute-cache-entry-cache-influences att-cache)))))
           (call-with-values
            (lambda ()
              (hashtable-entries (attribute-instance-cache att)))
            (lambda (key-vector value-vector)
              value-vector))))
        (node-attributes n*))
       (unless (node-terminal? n*)
         (for-each
          loop
          (node-children n*))))))
 
 (define rewrite-terminal
   (lambda (i n new-value)
     ;;; Before changing the value of the terminal ensure, that...
     (when (evaluator-state-in-evaluation? (node-evaluator-state n)) ; ...no attributes are in evaluation and...
       (throw-exception
        "Cannot change terminal value; "
        "There are attributes in evaluation."))
     (let ((n
            (if (symbol? i)
                (node-find-child n i)
                (and (>= i 1) (<= i (length (node-children n))) (list-ref (node-children n) (- i 1))))))
       (unless (and n (node-terminal? n)) ; ...the given context is a terminal.
         (throw-exception
          "Cannot change terminal value; "
          "The given context does not exist or is no terminal."))
       ;;; Everything is fine. Thus,...
       (let ((old-value (node-children n)))
         (for-each ; ...flush all attribute cache entries influenced by the terminal,...
          (lambda (influence)
            (flush-attribute-cache-entry (car influence)))
          (node-cache-influences n))
         (node-children-set! n new-value) ; ...rewrite its value and...
         old-value)))) ; ...return its old value.
 
 (define rewrite-refine
   (lambda (n t . c)
     ;;; Before refining the non-terminal node ensure, that...
     (when (evaluator-state-in-evaluation? (node-evaluator-state n)) ; ...non of its attributes are in evaluation,...
       (throw-exception
        "Cannot refine node; "
        "There are attributes in evaluation."))
     (when (or (node-list-node? n) (node-bud-node? n)) ; ...it is not a list or bud node,...
       (throw-exception
        "Cannot refine node; "
        "The node is a " (if (node-list-node? n) "list" "bud") " node."))
     (let* ((old-rule (node-ast-rule n))
            (new-rule (racr-specification-find-rule (ast-rule-specification old-rule) t)))
       (unless (and new-rule (ast-rule-subtype? new-rule old-rule)) ; ...the given type is a subtype and...
         (throw-exception
          "Cannot refine node; "
          t " is not a subtype of " (symbol-name (car (ast-rule-production old-rule))) "."))
       (let ((additional-children (list-tail (ast-rule-production new-rule) (length (ast-rule-production old-rule)))))
         (unless (satisfies-contexts? c additional-children) ; ...all additional children fit.
           (throw-exception
            "Cannot refine node; "
            "The given additional children do not fit."))
         ;;; Everything is fine. Thus,...
         (for-each ; ...flush the influenced attribute cache entries, i.e., all entries influenced by the node's...
          (lambda (influence)
            (flush-attribute-cache-entry (car influence)))
          (filter
           (lambda (influence)
             (or
              (and (vector-ref (cdr influence) 2) (not (null? c))) ; ...number of children,...
              (and (vector-ref (cdr influence) 3) (not (eq? old-rule new-rule))) ; ...type,...
              (find ; ...supertype,...
               (lambda (t2)
                 (not (eq? (ast-rule-subtype? t2 old-rule) (ast-rule-subtype? t2 new-rule))))
               (vector-ref (cdr influence) 4))
              (find ; ...subtype or...
               (lambda (t2)
                 (not (eq? (ast-rule-subtype? old-rule t2) (ast-rule-subtype? new-rule t2))))
               (vector-ref (cdr influence) 5))
              (find ; ...defined contexts and...
               (lambda (context-name)
                 (let ((old-defines-context? (ast-rule-find-child-context old-rule context-name))
                       (new-defines-context? (ast-rule-find-child-context new-rule context-name)))
                   (if old-defines-context? (not new-defines-context?) new-defines-context?)))
               (vector-ref (cdr influence) 6))))
           (node-cache-influences n)))
         (for-each ; ...all entries depending on the new children being roots. Afterwards,...
          (lambda (child context)
            (when (symbol-non-terminal? context)
              (for-each
               (lambda (influence)
                 (flush-attribute-cache-entry (car influence)))
               (filter
                (lambda (influence)
                  (vector-ref (cdr influence) 1))
                (node-cache-influences child)))))
          c
          additional-children)
         (node-ast-rule-set! n new-rule) ; ...update the node's type,...
         (update-synthesized-attribution n) ; ...synthesized attribution,...
         (node-children-set! ; ...insert the new children and...
          n
          (append
           (node-children n)
           (map
            (lambda (child context)
              (let ((child
                     (if (symbol-non-terminal? context)
                         child
                         (make-node 'terminal n child))))
                (node-parent-set! child n)
                (distribute-evaluator-state (node-evaluator-state n) child) ; ...update their evaluator state and...
                child))
            c
            additional-children)))
         (for-each
          update-inherited-attribution ; ...inherited attribution.
          (node-children n))))))
 
 (define rewrite-abstract
   (lambda (n t)
     ;;; Before abstracting the node ensure, that...
     (when (evaluator-state-in-evaluation? (node-evaluator-state n)) ; ...no attributes are in evaluation,...
       (throw-exception
        "Cannot abstract node; "
        "There are attributes in evaluation."))
     (when (or (node-list-node? n) (node-bud-node? n)) ; ...the node is not a list or bud node,...
       (throw-exception
        "Cannot abstract node; "
        "The node is a " (if (node-list-node? n) "list" "bud") " node."))
     (let* ((old-rule (node-ast-rule n))
            (new-rule (racr-specification-find-rule (ast-rule-specification old-rule) t)))
       (unless (and new-rule (ast-rule-subtype? old-rule new-rule)) ; ...the new type is a supertype and...
         (throw-exception
          "Cannot abstract node; "
          t " is not a supertype of " (symbol-name (car (ast-rule-production old-rule))) "."))
       ; ...permitted in the context in which the node is:
       (unless (or (not (node-parent n)) (valid-replacement-candidate? n (create-ast-mockup new-rule)))
         (throw-exception
          "Cannot abstract node; "
          "Abstraction to type " t " not permitted by context."))
       ;;; Everything is fine. Thus,...
       (let* ((num-new-children (length (cdr (ast-rule-production new-rule))))
              (children-to-remove (list-tail (node-children n) num-new-children)))
         (for-each ; ...flush all influenced attribute cache entries, i.e., all entries influenced by the node's...
          (lambda (influence)
            (flush-attribute-cache-entry (car influence)))
          (filter
           (lambda (influence)
             (or
              (and (vector-ref (cdr influence) 2) (not (null? children-to-remove))) ; ...number of children,...
              (and (vector-ref (cdr influence) 3) (not (eq? old-rule new-rule))) ; ...type...
              (find ; ...supertype,...
               (lambda (t2)
                 (not (eq? (ast-rule-subtype? t2 old-rule) (ast-rule-subtype? t2 new-rule))))
               (vector-ref (cdr influence) 4))
              (find ; ...subtype or...
               (lambda (t2)
                 (not (eq? (ast-rule-subtype? old-rule t2) (ast-rule-subtype? new-rule t2))))
               (vector-ref (cdr influence) 5))
              (find ; ...defined contexts and...
               (lambda (context-name)
                 (let ((old-defines-context? (ast-rule-find-child-context old-rule context-name))
                       (new-defines-context? (ast-rule-find-child-context new-rule context-name)))
                   (if old-defines-context? (not new-defines-context?) new-defines-context?)))
               (vector-ref (cdr influence) 6))))
           (node-cache-influences n)))
         (for-each ; ...all entries cross-depending the removed ASTs. Afterwards,...
          (lambda (child-to-remove)
            (flush-inter-fragment-dependent-attribute-cache-entries child-to-remove #t))
          children-to-remove)
         (node-ast-rule-set! n new-rule) ; ...update the node's type and its...
         (update-synthesized-attribution n) ; ...synthesized (because of possibly less) and...
         (update-inherited-attribution n) ; ...inherited (because of unshadowed) attributes. Further,...
         (for-each ; ...for every child to remove,...
          (lambda (child)
            (detach-inherited-attributes child) ; ...delete its inherited attributes,...
            (node-parent-set! child #f) ; ...detach it from the AST and...
            (distribute-evaluator-state (make-evaluator-state) child)) ; ...update its evaluator state. Then,...
          children-to-remove)
         (unless (null? children-to-remove)
           (if (> num-new-children 0)
               (set-cdr! (list-tail (node-children n) (- num-new-children 1)) (list))
               (node-children-set! n (list))))
         (for-each ; ...update the inherited attribution of all remaining children. Finally,...
          update-inherited-attribution
          (node-children n))
         (map ; ...return the removed children.
          (lambda (child) (if (node-terminal? child) (node-children child) child))
          children-to-remove)))))
 
 (define rewrite-subtree
   (lambda (old-fragment new-fragment)
     ;;; Before replacing the subtree ensure, that no attributes of the old fragment are in evaluation and...
     (when (evaluator-state-in-evaluation? (node-evaluator-state old-fragment))
       (throw-exception
        "Cannot replace subtree; "
        "There are attributes in evaluation."))
     (unless (valid-replacement-candidate? old-fragment new-fragment) ; ...the new fragment fits in its context.
       (throw-exception
        "Cannot replace subtree; "
        "The replacement does not fit."))
     ;;; When all rewrite constraints are satisfied,...
     (detach-inherited-attributes old-fragment) ; ...delete the old fragment's inherited attribution. Then,...
     ; ...flush all attribute cache entries cross-depending the old fragment and...
     (flush-inter-fragment-dependent-attribute-cache-entries old-fragment #t)
     (for-each ; ...all entries depending on the new fragment being a root. Afterwards,...
      (lambda (influence)
        (flush-attribute-cache-entry (car influence)))
      (filter
       (lambda (influence)
         (vector-ref (cdr influence) 1))
       (node-cache-influences new-fragment)))
     (distribute-evaluator-state (node-evaluator-state old-fragment) new-fragment) ; ...update both fragments' evaluator state,...
     (distribute-evaluator-state (make-evaluator-state) old-fragment)
     (set-car! ; ...replace the old fragment by the new one and...
      (list-tail (node-children (node-parent old-fragment)) (- (node-child-index? old-fragment) 1))
      new-fragment)
     (node-parent-set! new-fragment (node-parent old-fragment))
     (node-parent-set! old-fragment #f)
     (update-inherited-attribution new-fragment) ; ...update the new fragment's inherited attribution. Finally,...
     old-fragment)) ; ...return the removed old fragment.

 (define rewrite-add
   (lambda (l e)
     ;;; Before adding the element ensure, that...
     (when (evaluator-state-in-evaluation? (node-evaluator-state l)) ; ...no attributes of the list are in evaluation,...
       (throw-exception
        "Cannot add list element; "
        "There are attributes in evaluation."))
     (unless (node-list-node? l) ; ...indeed a list is given as context and...
       (throw-exception
        "Cannot add list element; "
        "The given context is no list-node."))
     (unless (valid-list-element-candidate? l e) ; ...the new element fits.
       (throw-exception
        "Cannot add list element; "
        "The new element does not fit."))
     ;;; When all rewrite constraints are satisfied,...
     (for-each ; ...flush all attribute cache entries influenced by the list-node's number of children and...
      (lambda (influence)
        (flush-attribute-cache-entry (car influence)))
      (filter
       (lambda (influence)
         (vector-ref (cdr influence) 2))
       (node-cache-influences l)))
     (for-each ; ...all entries depending on the new element being a root. Afterwards,...
      (lambda (influence)
        (flush-attribute-cache-entry (car influence)))
      (filter
       (lambda (influence)
         (vector-ref (cdr influence) 1))
       (node-cache-influences e)))
     (node-children-set! l (append (node-children l) (list e))) ; ...add the new element,...
     (node-parent-set! e l)
     (distribute-evaluator-state (node-evaluator-state l) e) ; ...initialize its evaluator state and...
     (when (node-parent l)
       (update-inherited-attribution e)))) ; ...any inherited attributes defined for its new context.
 
 (define rewrite-insert
   (lambda (l i e)
     ;;; Before inserting the new element ensure, that...
     (when (evaluator-state-in-evaluation? (node-evaluator-state l)) ; ...no attributes of the list are in evaluation,...
       (throw-exception
        "Cannot insert list element; "
        "There are attributes in evaluation."))
     (unless (node-list-node? l) ; ...indeed a list is given as context,...
       (throw-exception
        "Cannot insert list element; "
        "The given context is no list-node."))
     (when (or (< i 1) (> i (+ (length (node-children l)) 1))) ; ...the list has enough elements and...
       (throw-exception
        "Cannot insert list element; "
        "The given index is out of range."))
     (unless (valid-list-element-candidate? l e) ; ...the new element fits.
       (throw-exception
        "Cannot add list element; "
        "The new element does not fit."))
     ;;; When all rewrite constraints are satisfied...
     (for-each ; ...flush all attribute cache entries influenced by the list's number of children. Further,...
      (lambda (influence)
        (flush-attribute-cache-entry (car influence)))
      (filter
       (lambda (influence)
         (vector-ref (cdr influence) 2))
       (node-cache-influences l)))
     (for-each ; ...for each successor element after insertion,...
      (lambda (successor)
        (for-each ; ...flush all attribute cache entries depending on the respective element...
         (lambda (influence)
           (define query-direction? (vector-ref (cdr influence) 0)) ; ...via a downwards query. Then,...
           (when (or (eq? query-direction? 'down) (eq? query-direction? 'up/down))
             (flush-attribute-cache-entry (car influence))))
         (node-cache-influences successor)))
      (list-tail (node-children l) (- i 1)))
     (for-each ; ...flush all attribute cache entries depending on the new element being a root. Afterwards,...
      (lambda (influence)
        (flush-attribute-cache-entry (car influence)))
      (filter
       (lambda (influence)
         (vector-ref (cdr influence) 1))
       (node-cache-influences e)))
     (cond ; ...insert the new element,...
       ((null? (node-children l))
        (node-children-set! l (list e)))
       ((= (length (node-children l)) (- i 1))
        (node-children-set! l (append (node-children l) (list e))))
       (else
        (let ((insert-head (list-tail (node-children l) (- i 1))))
          (set-cdr! insert-head (cons (car insert-head) (cdr insert-head)))
          (set-car! insert-head e))))
     (node-parent-set! e l)
     (distribute-evaluator-state (node-evaluator-state l) e) ; ...initialize its evaluator state and...
     (when (node-parent l)
       (update-inherited-attribution e)))) ; ...any inherited attributes defined for its new context.
 
 (define rewrite-delete
   (lambda (n)
     ;;; Before deleting the element ensure, that...
     (when (evaluator-state-in-evaluation? (node-evaluator-state n)) ; ...no attributes are in evaluation and...
       (throw-exception
        "Cannot delete list element; "
        "There are attributes in evaluation."))
     (unless (and (node-parent n) (node-list-node? (node-parent n))) ; ...the given node is element of a list.
       (throw-exception
        "Cannot delete list element; "
        "The given node is not element of a list."))
     ;;; When all rewrite constraints are satisfied,...
     (detach-inherited-attributes n) ; ...delete the element's inherited attributes and...
     (for-each ;  ...flush all attribute cache entries influenced by...
      (lambda (influence)
        (flush-attribute-cache-entry (car influence)))
      (filter
       (lambda (influence)
         (or (vector-ref (cdr influence) 2) ; ...the number of children of the list node the element is part of or...
             (let ((query-direction? (vector-ref (cdr influence) 0))) ; ...that query the list node via...
               (and (or (eq? query-direction? 'up) (eq? query-direction? 'up/down)) ; ...an upwards query and...
                    (node-inside-of? ; ...are within the element's subtree. Also flush,...
                     (attribute-instance-context (attribute-cache-entry-context (car influence)))
                     n)))))
       (node-cache-influences (node-parent n))))
     (for-each ; ...for the element itself and each successor element,...
      (lambda (successor)
        (for-each ; ...all attribute cache entries depending on the respective element...
         (lambda (influence)
           (define query-direction? (vector-ref (cdr influence) 0)) ; ...via a downwards query. Finally,...
           (when (or (eq? query-direction? 'down) (eq? query-direction? 'up/down))
             (flush-attribute-cache-entry (car influence))))
         (node-cache-influences successor)))
      (list-tail (node-children (node-parent n)) (- (node-child-index? n) 1)))
     (node-children-set! (node-parent n) (remq n (node-children (node-parent n)))) ; ...remove the element from the list,...
     (node-parent-set! n #f)
     (distribute-evaluator-state (make-evaluator-state) n) ; ...reset its evaluator state and...
     n)) ; ...return it.
 
 (define perform-rewrites
   (lambda (n strategy . transformers)
     (define root
       (let loop ((n n))
         (if (ast-has-parent? n)
             (loop (ast-parent n))
             n)))
     (define root-deleted/inserted?
       (let ((evaluator-state (node-evaluator-state root)))
         (lambda ()
           (not (eq? evaluator-state (node-evaluator-state root))))))
     (define find-and-apply
       (case strategy
         ((top-down)
          (lambda (n)
            (and
             (not (node-terminal? n))
             (or
              (find (lambda (transformer) (transformer n)) transformers)
              (find find-and-apply (node-children n))))))
         ((bottom-up)
          (lambda (n)
            (and
             (not (node-terminal? n))
             (or
              (find find-and-apply (node-children n))
              (find (lambda (transformer) (transformer n)) transformers)))))
         (else (throw-exception
                "Cannot perform rewrites; "
                "Unknown " strategy " strategy."))))
     (let loop ()
       (when (root-deleted/inserted?)
         (throw-exception
          "Cannot perform rewrites; "
          "A given transformer manipulated the root of the AST."))
       (let ((match (find-and-apply root)))
         (if match
             (cons match (loop))
             (list))))))
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Initialisation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 (when (= (specification->phase ast-language) 1)
   (load-ast-language)))
