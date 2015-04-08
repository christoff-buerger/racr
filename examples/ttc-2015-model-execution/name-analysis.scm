; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-model-execution name-analysis)
 (export
  find-in-context
  specify-name-analysis)
 (import (rnrs) (racr core) (ttc-2015-model-execution user-interface))
 
 (define (find-in-context n ->? ->*?)
   (define x (->? n))
   (ast-find-child (lambda (i n) (eq? (->? n) x)) (->*? n)))
 
 (define (specify-name-analysis)
   (with-specification
    pn
    
    (ag-rule
     P-Lookup
     (AtomicPetrinet
      (lambda (n name)
        (find-in-context n ->name ->Place*))))
    
    (ag-rule
     Place
     (Arc
      (lambda (n)
        (P-Lookup n (->place n)))))
    
    (ag-rule
     T-Lookup
     (AtomicPetrinet
      (lambda (n name)
        (find-in-context n ->name ->Transition*)))))))