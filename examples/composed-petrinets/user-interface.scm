; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (composed-petrinets user-interface)
 (export petrinet: transition: compose-petrinets:)
 (import (rnrs) (racr core) (atomic-petrinets user-interface)
         (composed-petrinets analyses) (composed-petrinets execution))
 
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
        net*)))))