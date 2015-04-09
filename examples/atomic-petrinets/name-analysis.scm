; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (atomic-petrinets name-analysis)
 (export find-valued specify-name-analysis)
 (import (rnrs) (racr core) (atomic-petrinets user-interface))
 
 (define (find-valued n value ->value)
   (ast-find-child (lambda (i n) (eq? (->value n) value)) n))
 
 (define (specify-name-analysis)
   (with-specification
    pn
    
    (ag-rule
     p-lookup
     (AtomicPetrinet
      (lambda (n name)
        (find-valued (->Place* n) name ->name))))
    
    (ag-rule
     place
     (Arc
      (lambda (n)
        (=p-lookup n (->place n)))))
    
    (ag-rule
     t-lookup
     (AtomicPetrinet
      (lambda (n name)
        (find-valued (->Transition* n) name ->name)))))))