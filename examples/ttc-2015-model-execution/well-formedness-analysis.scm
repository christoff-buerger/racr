; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-model-execution well-formedness-analysis)
 (export
  specify-well-formedness-analysis)
 (import (rnrs) (racr core) (ttc-2015-model-execution user-interface) (ttc-2015-model-execution name-analysis))
 
 (define (find-not-valid l)
   (ast-find-child (lambda (i n) (not (Valid? n))) l))
 
 (define (specify-well-formedness-analysis)
   (with-specification
    pn
    
    (ag-rule
     Valid?
     
     (Place
      (lambda (n)
        (eq? (P-Lookup n (->name n)) n)))
     
     (Transition
      (lambda (n)
        (and (eq? (T-Lookup n (->name n)) n)
             (not (find-not-valid (->In n)))
             (not (find-not-valid (->Out n))))))
     
     (Arc
      (lambda (n)
        (and (Place n)
             (eq? (find-in-context n ->place <-) n))))
     
     (AtomicPetrinet
      (lambda (n)
        (and (not (find-not-valid (->Place* n)))
             (not (find-not-valid (->Transition* n))))))))))