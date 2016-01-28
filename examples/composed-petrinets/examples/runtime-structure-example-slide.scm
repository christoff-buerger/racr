; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr core) (composed-petrinets user-interface) (composed-petrinets analyses))

(define (run-tests)
  (define net1
    (petrinet:
     net1 (p2) (p1)
     ((p1 'T1 'T1 'T1)
      (p2 'T1 'T1))
     (transition: trans1
                  ((p1 (t1 (eq? t1 'T1)) (t2 (eq? t2 'T2)) (t3 (eq? t3 'T3)))
                   (p2 (t4 (eq? t4 'T1)) (t5 (eq? t5 'T3))))
                  ((p2 'T1)))))
  
  (define net2
    (petrinet:
     net2 (p1) (p1)
     ((p1 'T2))
     (transition: trans1
                  ((p1 (t1 (eq? t1 'T2)) (t2 (eq? t2 'T1))))
                  ((p1 'T2)))
     (transition: trans2
                  ((p1 (t1 (eq? t1 'T3)) (t2 (eq? t2 'T3))))
                  ((p1 'T2)))))
  
  (define net3
    (petrinet:
     net3 (p2) (p1)
     ((p1 'T3 'T3)
      (p2 'T3))
     (transition: trans1
                  ((p1 (t (eq? t 'T1))))
                  ((p1 'T3)
                   (p2 'T3)))))
  
  (define net1-net2-net3
    (compose-petrinets:
     net1
     (compose-petrinets:
      net2
      net3
      ((net2 p1) (net3 p2)))
     ((net1 p1) (net2 p1))
     ((net3 p1) (net1 p2))))
  
  (assert
   (equal? (=fused-places (=p-lookup net1 'p1))
           (=fused-places (=p-lookup net2 'p1))))
  (assert
   (equal? (=fused-places (=p-lookup net1 'p1))
           (=fused-places (=p-lookup net3 'p2))))
  (assert
   (equal? (=fused-places (=p-lookup net1 'p2))
           (=fused-places (=p-lookup net3 'p1))))
  
  (assert (= (length (=fused-places (=p-lookup net1 'p1))) 3))
  (assert
   (and
    (member (=p-lookup net1 'p1)
            (=fused-places (=p-lookup net1 'p1)))
    (member (=p-lookup net2 'p1)
            (=fused-places (=p-lookup net1 'p1)))
    (member (=p-lookup net3 'p2)
            (=fused-places (=p-lookup net1 'p1)))))
  (assert (= (length (=fused-places (=p-lookup net1 'p2))) 2))
  (assert
   (and
    (member (=p-lookup net1 'p2)
            (=fused-places (=p-lookup net1 'p2)))
    (member (=p-lookup net3 'p1)
            (=fused-places (=p-lookup net1 'p2)))))
  
  (assert-enabled net1-net2-net3
                  '(net1 trans1)
                  '(net2 trans1)
                  '(net3 trans1))
  (fire-transition! (=t-lookup net2 'trans1))
  (assert-enabled net1-net2-net3
                  '(net1 trans1)
                  '(net2 trans1)
                  '(net3 trans1))
  (fire-transition! (=t-lookup net1 'trans1))
  (assert-enabled net1-net2-net3
                  '(net1)
                  '(net2)
                  '(net3 trans1))
  (fire-transition! (=t-lookup net3 'trans1))
  (assert-enabled net1-net2-net3
                  '(net1)
                  '(net2)
                  '(net3 trans1))
  (fire-transition! (=t-lookup net3 'trans1))
  (assert-enabled net1-net2-net3
                  '(net1)
                  '(net2 trans2)
                  '(net3))
  (fire-transition! (=t-lookup net2 'trans2))
  (assert-enabled net1-net2-net3
                  '(net1)
                  '(net2 trans1)
                  '(net3))
  (fire-transition! (=t-lookup net2 'trans1))
  (assert-enabled net1-net2-net3
                  '(net1)
                  '(net2)
                  '(net3)))

(initialise-petrinet-language)
(run-tests)