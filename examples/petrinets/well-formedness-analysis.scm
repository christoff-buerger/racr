; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (petrinets well-formedness-analysis)
 (export
  specify-well-formedness-analysis)
 (import (rnrs) (racr) (petrinets ast))
 
 (define unique-symbol-list?
   (lambda (l)
     (or
      (null? l)
      (and
       (not (memq (car l) (cdr l)))
       (unique-symbol-list? (cdr l))))))
 
 (define specify-well-formedness-analysis
   (lambda ()
     (with-specification
      petrinet-spec
      
      (ag-rule
       unique-places?
       (Petrinet
        (lambda (n)
          (unique-symbol-list?
           (map
            (lambda (place)
              (ast-child 'name place))
            (ast-children (ast-child 'Place* n)))))))
      
      (ag-rule
       unique-transitions?
       (Petrinet
        (lambda (n)
          (unique-symbol-list?
           (map
            (lambda (transition)
              (ast-child 'name transition))
            (ast-children (ast-child 'Transition* n)))))))
      
      (ag-rule
       unique-input-places?
       (Transition
        (lambda (n)
          (unique-symbol-list?
           (map
            (lambda (in)
              (ast-child 'place in))
            (ast-children (ast-child 'In n)))))))
      
      (ag-rule
       well-formed?
       (Petrinet
        (lambda (n)
          (and
           (att-value 'unique-places? n)
           (att-value 'unique-transitions? n)
           (for-all
               (lambda (transition)
                 (att-value 'unique-input-places? transition))
             (ast-children (ast-child 'Transition* n)))))))))))