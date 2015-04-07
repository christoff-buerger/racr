; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-model-execution name-analysis)
 (export
  specify-name-analysis)
 (import (rnrs) (racr core))
 
 (define specify-name-analysis
   (lambda (specification)
     (with-specification
      specification
      
      (ag-rule
       find-place
       (AtomicPetrinet
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
       find-transition
       (AtomicPetrinet
        (lambda (n name)
          (ast-find-child
           (lambda (i transition)
             (eq? (ast-child 'name transition) name))
           (ast-child 'Transition* n)))))))))