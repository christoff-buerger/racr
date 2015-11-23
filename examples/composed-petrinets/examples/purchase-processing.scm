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

(import (rnrs) (racr core) (composed-petrinets user-interface))

(define (make-buyer-net)
  (petrinet:
   buyer (invoice goods) (order payment)
   
   ((p1 'token))
   
   (transition:
    t1
    ((p1 (x #t)))
    ((order 'token)
     (p2 'token)
     (p3 'token)))
   
   (transition:
    t2
    ((p2 (x #t))
     (goods (y #t)))
    ((p4 'token)))
   
   (transition:
    t3
    ((p3 (x #t))
     (invoice (y #t)))
    ((p5 'token)))
   
   (transition:
    t4
    ((p5 (x #t)))
    ((p6 'token)
     (payment 'token)))
   
   (transition:
    t5
    ((p4 (x #t))
     (p6 (y #t)))
    ((p7 'token)))))

(define (make-seller-net)
  (petrinet:
   seller (order payment) (internal-order invoice)
   
   ((p1 'token))
   
   (transition:
    t1
    ((p1 (x #t))
     (order (y #t)))
    ((p2 'token)
     (internal-order 'token)))
   
   (transition:
    t2
    ((p2 (x #t)))
    ((invoice 'token)
     (p3 'token)))
   
   (transition:
    t3
    ((p3 (x #t))
     (payment (y #t)))
    ((p4 'token)))))

(define (make-warehouse-net)
  (petrinet:
   warehouse (internal-order) (goods)
   
   ((p1 'token))
   
   (transition:
    t1
    ((p1 (x #t))
     (internal-order (y #t)))
    ((p2 'token)))
   
   (transition:
    t2
    ((p2 (x #t)))
    ((goods 'token)
     (p3 'token)))))

(define (has-token? petrinet qualified-place)
  (not
   (null?
    (ast-children
     (ast-child
      'Token*
      (att-value
       'find-place
       (att-value
        'find-subnet
        petrinet
        (car qualified-place))
       (cdr qualified-place)))))))

(define (run-tests)
  (let* ((buyer (make-buyer-net))
         (seller (make-seller-net))
         (warehouse (make-warehouse-net))
         (buyer-seller
          (compose-petrinets:
           buyer
           seller
           ((buyer order) (seller order))
           ((seller invoice) (buyer invoice))
           ((buyer payment) (seller payment)))))
    
    (run-petrinet! buyer-seller)
    (assert (null? (att-value 'enabled? buyer-seller)))
    (assert (not (has-token? buyer-seller (cons 'buyer 'p1))))
    (assert (has-token? buyer-seller (cons 'buyer 'p2))) ; TOKEN
    (assert (not (has-token? buyer-seller (cons 'buyer 'p3))))
    (assert (not (has-token? buyer-seller (cons 'buyer 'p4))))
    (assert (not (has-token? buyer-seller (cons 'buyer 'p5))))
    (assert (has-token? buyer-seller (cons 'buyer 'p6))) ; TOKEN
    (assert (not (has-token? buyer-seller (cons 'buyer 'p7))))
    (assert (not (has-token? buyer-seller (cons 'buyer 'order))))
    (assert (not (has-token? buyer-seller (cons 'buyer 'invoice))))
    (assert (not (has-token? buyer-seller (cons 'buyer 'payment))))
    (assert (not (has-token? buyer-seller (cons 'buyer 'goods))))
    (assert (not (has-token? buyer-seller (cons 'seller 'p1))))
    (assert (not (has-token? buyer-seller (cons 'seller 'p2))))
    (assert (not (has-token? buyer-seller (cons 'seller 'p3))))
    (assert (has-token? buyer-seller (cons 'seller 'p4))) ; TOKEN
    (assert (not (has-token? buyer-seller (cons 'seller 'order))))
    (assert (not (has-token? buyer-seller (cons 'seller 'invoice))))
    (assert (not (has-token? buyer-seller (cons 'seller 'payment))))
    (assert (has-token? buyer-seller (cons 'seller 'internal-order))) ; TOKEN
    
    
    (let ((buyer-seller-warehouse
           (compose-petrinets:
            buyer-seller
            warehouse
            ((seller internal-order) (warehouse internal-order))
            ((warehouse goods) (buyer goods)))))
      
      (run-petrinet! buyer-seller-warehouse)
      (assert (null? (att-value 'enabled? buyer-seller-warehouse)))
      (assert (not (has-token? buyer-seller (cons 'buyer 'p1))))
      (assert (not (has-token? buyer-seller (cons 'buyer 'p2))))
      (assert (not (has-token? buyer-seller (cons 'buyer 'p3))))
      (assert (not (has-token? buyer-seller (cons 'buyer 'p4))))
      (assert (not (has-token? buyer-seller (cons 'buyer 'p5))))
      (assert (not (has-token? buyer-seller (cons 'buyer 'p6))))
      (assert (has-token? buyer-seller (cons 'buyer 'p7))) ; TOKEN
      (assert (not (has-token? buyer-seller (cons 'buyer 'order))))
      (assert (not (has-token? buyer-seller (cons 'buyer 'invoice))))
      (assert (not (has-token? buyer-seller (cons 'buyer 'payment))))
      (assert (not (has-token? buyer-seller (cons 'buyer 'goods))))
      (assert (not (has-token? buyer-seller (cons 'seller 'p1))))
      (assert (not (has-token? buyer-seller (cons 'seller 'p2))))
      (assert (not (has-token? buyer-seller (cons 'seller 'p3))))
      (assert (has-token? buyer-seller (cons 'seller 'p4))) ; TOKEN
      (assert (not (has-token? buyer-seller (cons 'seller 'order))))
      (assert (not (has-token? buyer-seller (cons 'seller 'invoice))))
      (assert (not (has-token? buyer-seller (cons 'seller 'payment))))
      (assert (not (has-token? buyer-seller (cons 'seller 'internal-order))))
      (assert (not (has-token? buyer-seller-warehouse (cons 'warehouse 'p1))))
      (assert (not (has-token? buyer-seller-warehouse (cons 'warehouse 'p2))))
      (assert (has-token? buyer-seller-warehouse (cons 'warehouse 'p3))) ; TOKEN
      (assert (not (has-token? buyer-seller-warehouse (cons 'warehouse 'internal-order))))
      (assert (not (has-token? buyer-seller-warehouse (cons 'warehouse 'goods)))))))

(initialise-petrinet-language)
(run-tests)