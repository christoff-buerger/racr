; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr core) (composed-petrinets user-interface))

(init-ast)
(init-attribution)

(let* ((net1
        (make-petrinet
         net1 (p2) (p1)
         ((p1 'T1 'T1 'T1)
          (p2 'T1 'T1))
         (make-transition
          trans1
          ((p1 (t1 (eq? t1 'T1)) (t2 (eq? t2 'T2)) (t3 (eq? t3 'T3)))
           (p2 (t4 (eq? t4 'T1)) (t5 (eq? t5 'T3))))
          ((p2 'T1)))))
       
       (net2
        (make-petrinet
         net2 (p1) (p1)
         ((p1 'T2))
         (make-transition
          trans1
          ((p1 (t1 (eq? t1 'T2)) (t2 (eq? t2 'T1))))
          ((p1 'T2)))
         (make-transition
          trans2
          ((p1 (t #f)))
          ((p1 'T2)))))
       
       (net3
        (make-petrinet
         net3 (p2) (p1)
         ((p1 'T3 'T3)
          (p2 'T3))
         (make-transition
          trans1
          ((p1 (t (eq? t 'T1))))
          ((p1 'T3)
           (p2 'T3)))))
       
       (net1-net2-net3
        (compose-petrinets
         net1
         (compose-petrinets
          net2
          net3
          (((net2 p1) (net3 p2))))
         (((net1 p1) (net2 p1))
          ((net3 p1) (net1 p2)))))
       
       (net1-p1 (att-value 'find-place net1 'p1))
       (net1-p2 (att-value 'find-place net1 'p2))
       (net2-p1 (att-value 'find-place net2 'p1))
       (net3-p1 (att-value 'find-place net3 'p1))
       (net3-p2 (att-value 'find-place net3 'p2))
       
       (net1-trans1 (att-value 'find-transition net1 'trans1))
       (net2-trans1 (att-value 'find-transition net2 'trans1))
       (net2-trans2 (att-value 'find-transition net2 'trans2))
       (net3-trans1 (att-value 'find-transition net3 'trans1))
       
       (fused-net1-p1 (att-value 'fused-places net1-p1))
       (fused-net1-p2 (att-value 'fused-places net1-p2))
       (fused-net2-p1 (att-value 'fused-places net2-p1))
       (fused-net3-p1 (att-value 'fused-places net3-p1))
       (fused-net3-p2 (att-value 'fused-places net3-p2)))
  
  (assert
   (and
    (equal?
     fused-net1-p1
     fused-net2-p1)
    (equal?
     fused-net1-p1
     fused-net3-p2)))
  (assert
   (equal?
    fused-net1-p2
    fused-net3-p1))
  
  (assert (= (length fused-net1-p1) 3))
  (assert
   (and
    (member net1-p1 fused-net1-p1)
    (member net2-p1 fused-net1-p1)
    (member net3-p2 fused-net1-p1)))
  (assert (= (length fused-net1-p2) 2))
  (assert
   (and
    (member net1-p2 fused-net1-p2)
    (member net3-p1 fused-net1-p2)))
  
  (assert (= (length (att-value 'enabled? net1-net2-net3)) 3))
  (assert (not (att-value 'enabled? net2-trans2)))
  (fire-transition! net2-trans1)
  
  (assert (= (length (att-value 'enabled? net1-net2-net3)) 3))
  (assert (not (att-value 'enabled? net2-trans2)))
  (fire-transition! net1-trans1)
  
  (assert (= (length (att-value 'enabled? net1-net2-net3)) 1))
  (fire-transition! net3-trans1)
  
  (assert (= (length (att-value 'enabled? net1-net2-net3)) 1))
  (fire-transition! net3-trans1)
  
  (assert (null? (att-value 'enabled? net1-net2-net3))))