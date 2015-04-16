; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (atomic-petrinets name-analysis)
 (export specify-name-analysis)
 (import (rnrs) (racr core) (atomic-petrinets query-support))
 
 #|(define (find-valued n value ->value)
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
        (find-valued (->Transition* n) name ->name))))))|#
 
 #|(define (find-valued n value ->value)
   (find (lambda (n) (eq? (->value n) value)) n))
 
 (define (specify-name-analysis)
   (with-specification
    pn
    
    (ag-rule
     p-lookup
     (AtomicPetrinet
      (lambda (n name)
        (find-valued (=places n) name ->name))))
    
    (ag-rule
     place
     (Arc
      (lambda (n)
        (=p-lookup n (->place n)))))
    
    (ag-rule
     t-lookup
     (AtomicPetrinet
      (lambda (n name)
        (find-valued (=transitions n) name ->name))))))|#
 
 (define (make-symbol-table decls ->key)
   (define table (make-eq-hashtable))
   (for-each (lambda (n) (hashtable-set! table (->key n) n)) decls)
   table)
 
 (define (specify-name-analysis)
   (with-specification
    pn
    (ag-rule place      (Arc            (lambda (n) (=p-lookup n (->place n)))))
    (ag-rule p-lookup   (AtomicPetrinet (lambda (n) (make-symbol-table (=places n) ->name))))
    (ag-rule t-lookup   (AtomicPetrinet (lambda (n) (make-symbol-table (=transitions n) ->name))))
    (ag-rule in-lookup  (Transition     (lambda (n) (make-symbol-table (=in-arcs n) ->place))))
    (ag-rule out-lookup (Transition     (lambda (n) (make-symbol-table (=out-arcs n) ->place)))))))