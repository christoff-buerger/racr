; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (atomic-petrinets user-interface)
 (export petrinet: transition: fire-transition! run-petrinet! assert-marking assert-enabled)
 (import (rnrs) (rnrs mutable-pairs) (racr core) (racr testing)
         (atomic-petrinets query-support)
         (atomic-petrinets ast-scheme)
         (atomic-petrinets name-analysis)
         (atomic-petrinets well-formedness-analysis)
         (atomic-petrinets enabled-analysis)
         (atomic-petrinets execution-semantics))
 
 ;;; Syntax:
 
 (define-syntax petrinet:
   (syntax-rules ()
     ((_ ((place start-marking ...) ...)
         transition ... )
      (let ((net
             (:AtomicPetrinet
              (list (:Place 'place (:Token start-marking) ...)
                    ...)
              (list transition
                    ...))))
        (unless (=valid? net)
          (throw-petrinets-exception "Cannot construct Petri net; The net is not well-formed."))
        net))))
 
 (define-syntax transition:
   (syntax-rules ()
     ((_ name
         ((input-place (variable matching-condition) ...) ...)
         ((output-place to-produce ...) ...))
      (:Transition
       'name
       (list (:Arc
              'input-place
              (list (lambda (variable) matching-condition) ...))
             ...)
       (list (:Arc
              'output-place
              (lambda (variable ... ...) (list to-produce ...)))
             ...)))))
 
 ;;; Testing:
 
 (define (assert-marking net . marking) ; Each marking is a list of a place followed by its tokens.
   (define marked (map (lambda (m) (=p-lookup net (car m))) marking))
   (define marked-marking (map cdr marking))
   (define !marked (filter (lambda (n) (not (memq n marked))) (->* (->Place* net))))
   (define !marked-marking (map (lambda (n) (list)) !marked))
   (define (check-place place expected-tokens)
     (define given-values (map ->value (->* (->Token* place))))
     (define-record-type nil-record (sealed #t)(opaque #t))
     (define Ok (make-nil-record))
     (for-each
      (lambda (expected-token)
        (let ((value-found? (member expected-token given-values)))
          (assert value-found?)
          (set-car! value-found? Ok)))
      expected-tokens)
     (assert (for-all nil-record? given-values)))
   (for-each check-place marked marked-marking)
   (for-each check-place !marked !marked-marking))
 
 (define (assert-enabled net . enabled)
   (define t-enabled (map (lambda (t) (=t-lookup net t)) enabled))
   (define t-!enabled (filter (lambda (t) (not (memq t t-enabled))) (->* (->Transition* net))))
   (assert (for-all =enabled? t-enabled))
   (assert (for-all (lambda (t) (not (=enabled? t))) t-!enabled))
   (assert-exception (for-all fire-transition! t-!enabled)))
 
 ;;; Initialisation:
 
 (when (= (specification->phase pn) 1)
   (specify-ast)
   (compile-ast-specifications pn 'AtomicPetrinet)
   (specify-name-analysis)
   (specify-well-formedness-analysis)
   (specify-enabled-analysis)
   (compile-ag-specifications pn)))