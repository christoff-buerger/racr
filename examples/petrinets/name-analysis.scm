; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (petrinets name-analysis)
 (export
  specify-name-analysis)
 (import (rnrs) (racr core))
 
 (define specify-name-analysis
   (lambda (petrinet-specification)
     (with-specification
      petrinet-specification
      
      (ag-rule
       find-transition
       (AtomicPetrinet
        (lambda (n name)
          (ast-find-child
           (lambda (i transition)
             (eq? (ast-child 'name transition) name))
           (ast-child 'Transition* n)))))
      
      (ag-rule
       find-place
       (AtomicPetrinet
        (lambda (n name)
          (ast-find-child
           (lambda (i place)
             (eq? (ast-child 'name place) name))
           (ast-child 'Place* n)))))
      
      (ag-rule
       find-outport
       (AtomicPetrinet
        (lambda (n name)
          (ast-find-child
           (lambda (i port)
             (and
              (eq? (ast-child 'place port) name)
              (ast-subtype? port 'OutPort)))
           (ast-child 'Port* n)))))
      
      (ag-rule
       find-inport
       (AtomicPetrinet
        (lambda (n name)
          (ast-find-child
           (lambda (i port)
             (and
              (eq? (ast-child 'place port) name)
              (ast-subtype? port 'InPort)))
           (ast-child 'Port* n)))))
      
      (ag-rule
       find-subnet
       
       (ComposedPetrinet
        (lambda (n name)
          (or
           (att-value 'find-subnet (ast-child 'Net1 n) name)
           (att-value 'find-subnet (ast-child 'Net2 n) name))))
       
       (AtomicPetrinet
        (lambda (n name)
          (and (eq? (ast-child 'name n) name) n))))
      
      (ag-rule
       place
       
       (Arc
        (lambda (n)
          (att-value 'find-place n (ast-child 'place n))))
       
       (Port
        (lambda (n)
          (att-value 'find-place n (ast-child 'place n)))))))))