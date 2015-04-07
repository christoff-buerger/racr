; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-model-execution well-formedness-analysis)
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
   (lambda (specification)
     (with-specification
      specification
      
      (ag-rule
       well-formed?
       
       ; A place is well-formed, if its name is unique.
       (Place
        (lambda (n)
          (unique-node/terminal n 'name)))
       
       ; A transition is well-formed, if its name is unique and its arcs are well-formed:
       (Transition
        (lambda (n)
          (and
           (unique-node/terminal n 'name)
           (not
            (find
             (lambda (arc)
               (not (att-value 'well-formed? arc)))
             (append
              (ast-children (ast-child 'In n))
              (ast-children (ast-child 'Out n))))))))
       
       ; An arc is well-formed, if its designated place exits and is atmost once source
       ; and/or target of any arc of the transition the arc is part of:
       (Arc
        (lambda (n)
          (and
           (unique-node/terminal n 'place)
           (att-value 'place n))))
       
       (AtomicPetrinet
        (lambda (n)
          (not
           (find
            (lambda (n)
              (not (att-value 'well-formed? n)))
            (append
             (ast-children (ast-child 'Place* n))
             (ast-children (ast-child 'Transition* n))
             (ast-children (ast-child 'Port* n))))))))))))