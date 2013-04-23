; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (petrinets name-analysis)
 (export
  specify-name-analysis)
 (import (rnrs) (racr) (petrinets ast))
 
 (define specify-name-analysis
   (lambda ()
     (with-specification
      petrinet-spec
      
      (ag-rule
       find-transition
       (Petrinet
        (lambda (n name)
          (ast-find-child
           (lambda (i transition)
             (eq? (ast-child 'name transition) name))
           (ast-child 'Transition* n)))))
      
      (ag-rule
       find-place
       (Petrinet
        (lambda (n name)
          (ast-find-child
           (lambda (i place)
             (eq? (ast-child 'name place) name))
           (ast-child 'Place* n)))))
      
      (ag-rule
       place
       (Arc
        (lambda (n)
          (att-value 'find-place n (ast-child 'place n)))))
      
      (ag-rule
       input-places
       (Transition
        (lambda (n)
          (map
           (lambda (in)
             (att-value 'place in))
           (ast-children (ast-child 'In n))))))
      
      (ag-rule
       output-places
       (Transition
        (lambda (n)
          (map
           (lambda (out)
             (att-value 'place out))
           (ast-children (ast-child 'Out n))))))))))