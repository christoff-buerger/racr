; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (petrinets well-formedness-analysis)
 (export
  specify-well-formedness-analysis)
 (import (rnrs) (racr core))
 
 ; Given a node n (which usually is child of a list-node) and a child name t
 ; (which usually is a terminal), return, if all siblings of n with the same
 ; type as n have a different value for t than n has.
 (define unique-node/terminal
   (lambda (n t)
     (let ((value (ast-child t n)))
       (not
        (ast-find-child
         (lambda (i sibling)
           (and
            (not (eq? sibling n))
            (eq? (ast-node-type sibling) (ast-node-type n))
            (equal? (ast-child t sibling) value)))
         (ast-parent n))))))
 
 (define specify-well-formedness-analysis
   (lambda (petrinet-specification)
     (with-specification
      petrinet-specification
      
      (ag-rule
       well-formed?
       
       ; A place is well-formed, if its name is unique within the atomic Petri
       ; net N it is part of and the places it is fused with are not part of N:
       (Place
        (lambda (n)
          (and
           (unique-node/terminal n 'name)
           (not
            (find
             (lambda (fused-place)
               (and
                (not (eq? fused-place n))
                (eq?
                 (att-value 'containing-petrinet fused-place)
                 (att-value 'containing-petrinet n))))
             (att-value 'fused-places n))))))
       
       ; A transition is well-formed, if its name is unique within the atomic
       ; Petri net it is part of and its arcs are well-formed:
       (Transition
        (lambda (n)
          (and
           (unique-node/terminal n 'name)
           (not
            (find
             (lambda (arc)
               (not (att-value 'well-formed? arc)))
             (append (ast-children (ast-child 'In n)) (ast-children (ast-child 'Out n))))))))
       
       ; An arc is well-formed, if its designated place exits and is atmost once source
       ; and/or target of any arc of the transition the arc is part of:
       (Arc
        (lambda (n)
          (and
           (unique-node/terminal n 'place)
           (att-value 'place n))))
       
       ; A port is well-formed if it is unique within the atomic petri net it is
       ; part of and its designated place exists:
       (Port
        (lambda (n)
          (and
           (unique-node/terminal n 'place)
           (att-value 'place n))))
       
       ; A non-composed Petri net is well-formed, iff its places, transitions
       ; and ports are well-formed and its name is unique:
       (AtomicPetrinet
        (lambda (n)
          (and
           (not
            (find
             (lambda (n)
               (not (att-value 'well-formed? n)))
             (append
              (ast-children (ast-child 'Place* n))
              (ast-children (ast-child 'Transition* n))
              (ast-children (ast-child 'Port* n)))))
           (eq?
            (att-value
             'find-subnet
             (let loop ((n n))
               (if (ast-child 'issubnet n)
                   (loop (ast-parent n))
                   n))
             (ast-child 'name n))
            n))))
       
       ; A glueing is well-formed, if its input and outport port are glued atmost once
       ; within the composed petri net it is part of and these ports exist:
       (Glueing
        (lambda (n)
          (and
           (unique-node/terminal n 'outport)
           (unique-node/terminal n 'inport)
           (att-value 'outport n)
           (att-value 'inport n))))
       
       ; A composed Petri net is well-formed, if its subnets and glueings are well-formed:
       (ComposedPetrinet
        (lambda (n)
          (and
           (att-value 'well-formed? (ast-child 'Net1 n))
           (att-value 'well-formed? (ast-child 'Net2 n))
           (not
            (ast-find-child
             (lambda (i glueing)
               (not (att-value 'well-formed? glueing)))
             (ast-child 'Glueing* n)))))))))))