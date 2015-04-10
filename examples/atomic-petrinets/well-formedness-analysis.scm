; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (atomic-petrinets well-formedness-analysis)
 (export specify-well-formedness-analysis)
 (import (rnrs) (racr core) (atomic-petrinets query-support) (atomic-petrinets name-analysis))
 
 (define (find-not-valid l)
   (ast-find-child (lambda (i n) (not (=valid? n))) l))
 
 (define (specify-well-formedness-analysis)
   (with-specification
    pn
    
    (ag-rule
     valid?
     
     (Place
      (lambda (n)
        (eq? (=p-lookup n (->name n)) n)))
     
     (Transition
      (lambda (n)
        (and (eq? (=t-lookup n (->name n)) n)
             (not (find-not-valid (->In n)))
             (not (find-not-valid (->Out n))))))
     
     (Arc
      (lambda (n)
        (define place (->place n))
        (and (=place n)
             (eq? (find-valued (<- n) (->place n) ->place) n))))
     
     (AtomicPetrinet
      (lambda (n)
        (and (not (find-not-valid (->Place* n)))
             (not (find-not-valid (->Transition* n))))))))))