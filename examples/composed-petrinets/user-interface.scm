; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (composed-petrinets user-interface)
 (export initialise-petrinet-language petrinet: compose-petrinets: =p-lookup =t-lookup
         fire-transition! run-petrinet! interpret-petrinet!
         petrinets-exception? assert-marking assert-enabled
         (rename (apnl:petrinet: transition:)))
 (import (rnrs) (racr core) (prefix (atomic-petrinets user-interface) apnl:)
         (composed-petrinets analyses) (composed-petrinets execution))

 ;;; Syntax:
 
 (define-syntax petrinet: ; BEWARE: Redefinition
   (syntax-rules ()
     ((_ name (inport ...) (outport ...)
         ((place start-marking ...) ...)
         transition ... )
      (let ((net
             (:AtomicPetrinet
              'name
              (list (:Place 'place (:Token start-marking) ...)
                    ...)
              (list transition
                    ...)
              (list (:Inport 'inport) ... (:Outport 'outport) ...))))
        (unless (=valid? net)
          (exception: "Cannot construct Petri net; The net is not well-formed."))
        net))))
 
 (define-syntax compose-petrinets:
   (syntax-rules ()
     ((_ net1 net2 ((out-net out-port) (in-net in-port)) ...)
      (let ((net* (:ComposedNet
                   net1 net2
                   (:Glueing (cons 'out-net 'out-port) (cons 'in-net 'in-port)) ...)))
        (unless (=valid? net*)
          (rewrite-subtree (->Net1 net*) (make-ast-bud))
          (rewrite-subtree (->Net2 net*) (make-ast-bud))
          (exception: "Cannot compose Petri nets; The composed net is not well-formed."))
        net*))))

 ;;; Execution:
 
 (define (run-petrinet! petrinet) ; BEWARE: Redefinition
   (unless (=valid? petrinet)
     (exception: "Cannot run Petri Net; The given net is not well-formed."))
   (let ((enabled? ((=subnet-iter petrinet) (lambda (name n) (find =enabled? (=transitions n))))))
     (when enabled?
       (fire-transition! enabled?)
       (run-petrinet! petrinet))))
 
 ;;; REPL Interpreter:

 ;;; Testing:
 
 ;;; Initialisation:
 
 (define (initialise-petrinet-language) ; BEWARE: Redefinition
   (when (= (specification->phase pn) 1)
     (specify-analyses)
     (compile-ag-specifications pn))))