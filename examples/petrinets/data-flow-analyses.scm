; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (petrinets data-flow-analyses)
 (export
  specify-data-flow-analyses)
 (import (rnrs) (racr) (petrinets ast))
 
 (define list-union
   (lambda (l1 l2)
     (append
      (filter
       (lambda (e1)
         (not (memq e1 l2)))
       l1)
      l2)))
 
 (define specify-data-flow-analyses
   (lambda ()
     (with-specification
      petrinet-spec
      
      (ag-rule
       directly-connected-places
       ; Every place is directly connected to itself and all places that are an
       ; input or output of a transition the place is an input or output of:
       ((Petrinet Place*)
        (lambda (n)
          (let ((transitions (ast-children (ast-child 'Transition* (ast-parent (ast-parent n))))))
            (fold-left
             (lambda (result transition)
               (list-union
                result
                (list-union
                 (att-value 'input-places transition)
                 (att-value 'output-places transition))))
             (list)
             (filter
              (lambda (transition)
                (or
                 (memq n (att-value 'input-places transition))
                 (memq n (att-value 'output-places transition))))
              transitions))))))
      
      (ag-rule
       connected-places
       ; Every place is connected to the places it is directly connected to
       ; and all places these places are connected to:
       (Place
        (lambda (n)
          (let ((directly-connected-places (att-value 'directly-connected-places n)))
            (fold-left
             (lambda (result place)
               (list-union result (att-value 'connected-places place)))
             directly-connected-places
             directly-connected-places)))
        (list)
        (lambda (r1 r2)
          (= (length r1) (length r2)))))
      
      (ag-rule
       connected?
       (Petrinet
        (lambda (n)
          (or
           (= (ast-num-children (ast-child 'Place* n)) 0)
           (=
            (length
             (att-value
              'connected-places
              (ast-child 1 (ast-child 'Place* n))))
            (ast-num-children (ast-child 'Place* n)))))))))))