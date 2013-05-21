; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

; The implemented port concept for Petri net composition is based on
; 
;                               "Simple Composition of Nets"
;                                     Wolfgang Reisig
;          Applications and Theory of Petri Nets: 30th International Conference
;               Lecture Notes in Computer Science, Volume 5606, Pages 23-42
;                                   Springer, June 2009
;                      Editors: Giuliana Franceschinis, Karsten Wolf
;                                    978-3-642-02423-8
; 
; The implementation differs in three details:
;  1. To avoid the indexing problem, ports are not fused by name equivalence, but rather
;     user-specified explicit glueing of in- and out-ports.
;  2. The glueing of ports of the same type (e.g., two in-ports) is not permitted. Instead
;     it must be explicitely modeled by constructing a special glueing net that is
;     composed with the in- or out-ports to fuse. E.g., consider Fig. 8, where two
;     equivalent warehouses are composed. In the example the two order in-ports and two
;     goods out-ports of the two warehouses are fused, such that the composed warehouse
;     has only one order in-port and one goods out-port. The same can be modeled more
;     precisely, by first constructing a new glueing Petri net that only has one order
;     in-port and one goods out-port and additionally two artificial out-ports orders1 and
;     orders2 and two artificial in-ports goods1 and goods2. Two transitions connect the
;     three order and goods places respectively. The order related transition models
;     whether the order1 and order2 ports compete for token or not (i.e., the two
;     warehouses represent alternative or parallel processes). Similarly, the goods related
;     transition models whether the warehouses are synchronized or just the first
;     delivering any good succeeds. By composing the glue net with the two warehouses,
;     the meaning of the fusion of the warehouses' in- and out-ports becomes clear.
;  3. The fusion of different places of the same atomic petrinet is not permitted. The
;     reason for this decision is two-fold. Technically, the implemented firing semantics
;     can fail in such cases. Logically, the structure of subnets should be immutable.
;     After all they are black-box components. To fuse their places can result in
;     unexpected and unintended black-box behaviour.

#!r6rs

(library
 (petrinets composition-analysis)
 (export
  specify-composition-analysis)
 (import (rnrs) (racr) (petrinets ast))
 
 (define specify-composition-analysis
   (lambda ()
     (with-specification
      petrinet-spec
      
      (ag-rule
       fused-places
       (Place
        (lambda (n)
          (let* ((inport-glueing? (att-value 'is-glued-as-inport? n))
                 (fused-place? (and inport-glueing? (att-value 'place (att-value 'outport inport-glueing?)))))
            (if inport-glueing?
                (let ((fused-places (att-value 'fused-places fused-place?)))
                  (if (not (memq fused-place? fused-places))
                      (cons fused-place? fused-places)
                      fused-places))
                (list))))
        (list)
        (lambda (r1 r2)
          (= (length r1) (length r2)))))
      
      (ag-rule
       is-glued-as-inport?
       (Place
        (lambda (n)
          (let ((inport? (att-value 'find-inport n (ast-child 'name n))))
            (and
             inport?
             (att-value 'find-glueing-for-port n inport?))))))
      
      (ag-rule
       find-glueing-for-port
       
       (AtomicPetrinet
        (lambda (n port)
          (and
           (ast-child 'issubnet n)
           (att-value 'find-glueing-for-port (ast-parent n) port))))
       
       (ComposedPetrinet
        (lambda (n port)
          (let ((glueing?
                 (ast-find-child
                  (lambda (i glueing)
                    (or
                     (eq? (att-value 'outport glueing) port)
                     (eq? (att-value 'inport glueing) port)))
                  (ast-child 'Glueing* n))))
            (or
             glueing?
             (and
              (ast-child 'issubnet n)
              (att-value 'find-glueing-for-port (ast-parent n) port)))))))
      
      (ag-rule
       qualified-name
       ((AtomicPetrinet Port*)
        (lambda (n)
          (cons
           (ast-child 'name (ast-parent (ast-parent n)))
           (ast-child 'place n)))))
      
      (ag-rule
       outport
       (Glueing
        (lambda (n)
          (find
           (lambda (port)
             (and
              (equal? (att-value 'qualified-name port) (ast-child 'outport n))
              (ast-subtype? port 'OutPort)))
           (att-value 'available-ports n)))))
      
      (ag-rule
       inport
       (Glueing
        (lambda (n)
          (find
           (lambda (port)
             (and
              (equal? (att-value 'qualified-name port) (ast-child 'inport n))
              (ast-subtype? port 'InPort)))
           (att-value 'available-ports n)))))
      
      (ag-rule
       available-ports
       (ComposedPetrinet
        (lambda (n)
          (append
           (att-value 'exported-ports (ast-child 'Net1 n))
           (att-value 'exported-ports (ast-child 'Net2 n))))))
      
      (ag-rule
       exported-ports
       
       (ComposedPetrinet
        (lambda (n)
          (filter
           (lambda (port)
             (not
              (ast-find-child
               (lambda (i glueing)
                 (or
                  (eq? (att-value 'inport glueing) port)
                  (eq? (att-value 'outport glueing) port)))
               (ast-child 'Glueing* n))))
           (att-value 'available-ports n))))
       
       (AtomicPetrinet
        (lambda (n)
          (ast-children (ast-child 'Port* n)))))))))