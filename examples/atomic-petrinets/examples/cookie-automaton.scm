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

(import (rnrs)
        (atomic-petrinets main)
        (atomic-petrinets user-interface)
        (atomic-petrinets execution-semantics))

(define cookie-automaton
  (petrinet:
   ((H 'Box 'Box 'Box 'Box 'Box 'Box* 'Box*)
    (D 'Token)
    (G 'Token)
    (E 7))
   
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
