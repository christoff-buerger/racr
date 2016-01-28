; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

; Specification of the composition example given in
; 
;                                   "Simple Composition of Nets"
;                                         Wolfgang Reisig
;              Applications and Theory of Petri Nets: 30th International Conference
;                   Lecture Notes in Computer Science, Volume 5606, Pages 23-42
;                                       Springer, June 2009
;                          Editors: Giuliana Franceschinis, Karsten Wolf
;                                        978-3-642-02423-8
; 
; and on page 93, Figure 8.1 in
; 
;                 "Petrinetze: Modellierungstechnik, Analysemethoden, Fallstudien"
;                                         Wolfgang Reisig
;                                       Vieweg+Teubner, 2010
;                                        978-3-8348-1290-2

#!r6rs

(import (rnrs) (racr core) (composed-petrinets user-interface) (composed-petrinets analyses))

(define (make-buyer-net)
  (petrinet:
   buyer (invoice goods) (order payment)
   
   ((p1 'token) (p2) (p3) (p4) (p5) (p6) (p7) (order) (invoice) (payment) (goods))
   
   (transition: t1
                ((p1 (x #t)))
                ((order 'token)
                 (p2 'token)
                 (p3 'token)))
   
   (transition: t2
                ((p2 (x #t))
                 (goods (y #t)))
                ((p4 'token)))
   
   (transition: t3
                ((p3 (x #t))
                 (invoice (y #t)))
                ((p5 'token)))
   
   (transition: t4
                ((p5 (x #t)))
                ((p6 'token)
                 (payment 'token)))
   
   (transition: t5
                ((p4 (x #t))
                 (p6 (y #t)))
                ((p7 'token)))))

(define (make-seller-net)
  (petrinet:
   seller (order payment) (internal-order invoice)
   
   ((p1 'token) (p2) (p3) (p4) (order) (invoice) (payment) (internal-order))
   
   (transition: t1
                ((p1 (x #t))
                 (order (y #t)))
                ((p2 'token)
                 (internal-order 'token)))
   
   (transition: t2
                ((p2 (x #t)))
                ((invoice 'token)
                 (p3 'token)))
   
   (transition: t3
                ((p3 (x #t))
                 (payment (y #t)))
                ((p4 'token)))))

(define (make-warehouse-net)
  (petrinet:
   warehouse (internal-order) (goods)
   
   ((p1 'token) (p2) (p3) (internal-order) (goods))
   
   (transition: t1
                ((p1 (x #t))
                 (internal-order (y #t)))
                ((p2 'token)))
   
   (transition: t2
                ((p2 (x #t)))
                ((goods 'token)
                 (p3 'token)))))

(define (run-tests)
  (let* ((buyer (make-buyer-net))
         (seller (make-seller-net))
         (warehouse (make-warehouse-net))
         (buyer-seller
          (compose-petrinets:
           buyer
           seller
           ((buyer order)        ; Fuse order outport of buyer with...
            (seller order))      ; ...order inport of seller.
           ((seller invoice)     ; Dito for seller's and...
            (buyer invoice))     ; ...buyer's invoice and...
           ((buyer payment)      ; ...payment ports.
            (seller payment)))))
    
    (assert-enabled buyer-seller
                    '(buyer t1)
                    '(seller))
    (assert-marking buyer-seller
                    '(buyer (p1 token))
                    '(seller (p1 token)))
    (run-petrinet! buyer-seller)
    (assert-enabled buyer-seller
                    '(buyer)
                    '(seller))
    (assert-marking buyer-seller
                    '(buyer (p2 token) (p6 token))
                    '(seller (p4 token) (internal-order token)))
    
    (let ((buyer-seller-warehouse
           (compose-petrinets:
            buyer-seller
            warehouse
            ((seller internal-order) (warehouse internal-order))
            ((warehouse goods) (buyer goods)))))
      
      (assert-enabled buyer-seller-warehouse
                      '(buyer)
                      '(seller)
                      '(warehouse t1))
      (assert-marking buyer-seller-warehouse
                      '(buyer (p2 token) (p6 token))
                      '(seller (p4 token) (internal-order token))
                      '(warehouse (p1 token))) ; Fusion: (internal-order token)
      (run-petrinet! buyer-seller-warehouse)
      (assert-enabled buyer-seller-warehouse
                      '(buyer)
                      '(seller)
                      '(warehouse))
      (assert-marking buyer-seller-warehouse
                      '(buyer (p7 token))
                      '(seller (p4 token))
                      '(warehouse (p3 token))))))

(initialise-petrinet-language)
(run-tests)