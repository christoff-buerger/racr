; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-model-execution user-interface)
 (export pn petrinets-exception? throw-petrinets-exception
         make-Token
         ->Place* ->Transition* ->Token* ->In ->Out ->name ->value ->place ->consumers ->* <-
         P-Lookup T-Lookup Place Valid? Enabled?
         petrinet: transition:)
 (import (rnrs) (racr core))
 
 (define pn                   (create-specification))
 
 ; Attribute Accessors:
 (define (P-Lookup n name)    (att-value 'P-Lookup n name))
 (define (T-Lookup n name)    (att-value 'T-Lookup n name))
 (define (Place n)            (att-value 'Place n))
 (define (Valid? n)           (att-value 'Valid? n))
 (define (Enabled? n)         (att-value 'Enabled? n))
 
 ; AST Accessors:
 (define (->Place* n)         (ast-child 'Place* n))
 (define (->Transition* n)    (ast-child 'Transition* n))
 (define (->Token* n)         (ast-child 'Token* n))
 (define (->In n)             (ast-child 'In n))
 (define (->Out n)            (ast-child 'Out n))
 (define (->name n)           (ast-child 'name n))
 (define (->value n)          (ast-child 'value n))
 (define (->place n)          (ast-child 'place n))
 (define (->consumers n)      (ast-child 'consumers n))
 (define (->* n)              (ast-children n))
 (define (<- n)               (ast-parent n))
 
 ; AST Constructors:
 (define (make-Petrinet p t)
   (create-ast pn 'Petrinet (list (create-ast-list p) (create-ast-list t))))
 (define (make-Place n . t)
   (create-ast pn 'Place (list n (create-ast-list t))))
 (define (make-Token v)
   (create-ast pn 'Token (list v)))
 (define (make-Transition n i o)
   (create-ast pn 'Transition (list n (create-ast-list i) (create-ast-list o))))
 (define (make-Arc p f)
   (create-ast pn 'Arc (list p f)))
 
 ;;; Exceptions:
 
 (define-condition-type petrinets-exception
   &violation
   make-petrinets-exception
   petrinets-exception?)
 
 (define (throw-petrinets-exception message)
   (raise-continuable
    (condition
     (make-petrinets-exception)
     (make-message-condition message))))
 
 ;;; Syntax:
 
 (define (initialize-places petrinet)
   (ast-for-each-child
    (lambda (i trans)
      (for-each
       (lambda (arc)
         (unless (Place arc)
           (rewrite-add
            (->Place* petrinet)
            (make-Place (->place arc) (list)))))
       (append (->* (->In trans)) (->* (->Out trans)))))
    (->Transition* petrinet)))
 
 (define-syntax petrinet:
   (syntax-rules ()
     ((_ ((place start-marking ...) ...)
         transition ... )
      (let ((net
             (make-Petrinet
              (list (make-Place 'place (make-Token start-marking) ...)
                    ...)
              (list transition
                    ...))))
        (initialize-places net)
        (unless (Valid? net)
          (throw-petrinets-exception "Cannot construct Petri net; The net is not well-formed."))
        net))))
 
 (define-syntax transition:
   (syntax-rules ()
     ((_ name
         ((input-place (variable matching-condition) ...) ...)
         ((output-place to-produce ...) ...))
      (make-Transition
       'name
       (list (make-Arc
              'input-place
              (list (lambda (variable) matching-condition) ...))
             ...)
       (list (make-Arc
              'output-place
              (lambda (variable ... ...) (list to-produce ...)))
             ...))))))