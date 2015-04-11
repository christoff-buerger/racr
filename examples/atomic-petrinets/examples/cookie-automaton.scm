; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

; Specification of the cookie automaton given on page 21, Figure 2.1 in
; 
;               "Petrinetze: Modellierungstechnik, Analysemethoden, Fallstudien"
;                                     Wolfgang Reisig
;                                  Vieweg+Teubner, 2010
;                                    978-3-8348-1290-2

#!r6rs

(import (rnrs) (racr core) (racr testing)
        (atomic-petrinets user-interface))

(define (make-cookie-automaton)
  (petrinet:
   ((H 'Box 'Box 'Box 'Box 'Box 'Box* 'Box*)
    (D 'Token)
    (G 'Token)
    (E 7)
    (A) (B) (C) (F))
   
   (transition:
    c
    ((D (token (eq? token 'Token))))
    ((A 'Euro)))
   
   (transition:
    e
    ((A (euro (eq? euro 'Euro))))
    ((D 'Token)))
   
   (transition:
    a
    ((A (euro (eq? euro 'Euro)))
     (E (x (>= x 2)))
     (G (token (eq? token 'Token))))
    ((D 'Token)
     (E (- x 2))
     (F 'Euro)
     (B 'Token)))
   
   (transition:
    b
    ((B (token (eq? token 'Token)))
     (H (y #t) (z #t)))
    ((G 'Token)
     (C y z)))
   
   (transition:
    d
    ((C (y #t)))
    ())))

(define (run-tests)
  (let ((net (make-cookie-automaton)))
    (assert-enabled net 'c)
    (fire-transition! (=t-lookup net 'c))
    (assert-enabled net 'a 'e)
    (fire-transition! (=t-lookup net 'a))
    (assert-enabled net 'b 'c)
    (fire-transition! (=t-lookup net 'c))
    (assert-enabled net 'b 'e)
    (fire-transition! (=t-lookup net 'e))
    (assert-enabled net 'b 'c)
    (fire-transition! (=t-lookup net 'c))
    (assert-enabled net 'b 'e)
    (fire-transition! (=t-lookup net 'b))
    (assert-enabled net 'a 'd 'e)
    (fire-transition! (=t-lookup net 'e))
    (assert-enabled net 'c 'd)
    (fire-transition! (=t-lookup net 'c))
    (assert-enabled net 'a 'd 'e)
    (fire-transition! (=t-lookup net 'd))
    (assert-enabled net 'a 'e 'd)
    (fire-transition! (=t-lookup net 'd))
    (assert-enabled net 'a 'e)
    (fire-transition! (=t-lookup net 'a))
    (assert-enabled net 'b 'c)
    (fire-transition! (=t-lookup net 'b))
    (assert-enabled net 'c 'd)
    (fire-transition! (=t-lookup net 'c))
    (assert-enabled net 'a 'd 'e)
    (fire-transition! (=t-lookup net 'a))
    (assert-enabled net 'b 'c 'd)
    (fire-transition! (=t-lookup net 'b))
    (assert-enabled net 'c 'd)
    (fire-transition! (=t-lookup net 'c))
    (assert-enabled net 'd 'e)
    (fire-transition! (=t-lookup net 'd))
    (assert-enabled net 'd 'e)
    (fire-transition! (=t-lookup net 'd))
    (assert-enabled net 'd 'e)
    (fire-transition! (=t-lookup net 'd))
    (assert-enabled net 'd 'e)
    (fire-transition! (=t-lookup net 'd))
    (assert-enabled net 'e)
    (fire-transition! (=t-lookup net 'e))
    (assert-enabled net 'c)
    (fire-transition! (=t-lookup net 'c))
    (assert-enabled net 'e)
    (fire-transition! (=t-lookup net 'e))
    (assert-enabled net 'c)
    (fire-transition! (=t-lookup net 'c))
    (assert-enabled net 'e)
    (fire-transition! (=t-lookup net 'e))
    ; ...
    (assert-marking
     net
     (list 'D 'Token)
     (list 'E 1)
     (list 'F 'Euro 'Euro 'Euro)
     (list 'G 'Token)
     (list 'H 'Box*)))
  
  (let ((net (make-cookie-automaton)))
    (rewrite-delete (=t-lookup net 'e))
    (run-petrinet! net)
    (assert-marking
     net
     (list 'A 'Euro)
     (list 'E 1)
     (list 'F 'Euro 'Euro 'Euro)
     (list 'G 'Token)
     (list 'H 'Box*)))
  
  (let ((net (make-cookie-automaton)))
    (rewrite-delete (=p-lookup net 'A))
    (assert-exception petrinets-exception? (run-petrinet! net))))

(run-tests)
