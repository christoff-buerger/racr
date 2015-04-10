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
        (atomic-petrinets query-support)
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
  (define net                 (make-cookie-automaton))
  (define (marking! . m)      (apply assert-marking net m))
  (define (enabled! . t)      (apply assert-enabled net t))
  (define (fire! t)           (fire-transition! (=t-lookup net t)))
  
  (enabled! 'c)
  (fire! 'c)
  (enabled! 'a 'e)
  (fire! 'a)
  (enabled! 'b 'c)
  (fire! 'c)
  (enabled! 'b 'e)
  (fire! 'e)
  (enabled! 'b 'c)
  (fire! 'c)
  (enabled! 'b 'e)
  (fire! 'b)
  (enabled! 'a 'd 'e)
  (fire! 'e)
  (enabled! 'c 'd)
  (fire! 'c)
  (enabled! 'a 'd 'e)
  (fire! 'd)
  (enabled! 'a 'e 'd)
  (fire! 'd)
  (enabled! 'a 'e)
  (fire! 'a)
  (enabled! 'b 'c)
  (fire! 'b)
  (enabled! 'c 'd)
  (fire! 'c)
  (enabled! 'a 'd 'e)
  (fire! 'a)
  (enabled! 'b 'c 'd)
  (fire! 'b)
  (enabled! 'c 'd)
  (fire! 'c)
  (enabled! 'd 'e)
  (fire! 'd)
  (enabled! 'd 'e)
  (fire! 'd)
  (enabled! 'd 'e)
  (fire! 'd)
  (enabled! 'd 'e)
  (fire! 'd)
  (enabled! 'e)
  (fire! 'e)
  (enabled! 'c)
  (fire! 'c)
  (enabled! 'e)
  (fire! 'e)
  (enabled! 'c)
  (fire! 'c)
  (enabled! 'e)
  (fire! 'e)
  ; ...
  (marking! (list 'D 'Token)
            (list 'E 1)
            (list 'F 'Euro 'Euro 'Euro)
            (list 'G 'Token)
            (list 'H 'Box*))
  
  (set! net (make-cookie-automaton))
  (rewrite-delete (=t-lookup net 'e))
  (run-petrinet! net)
  (marking! (list 'A 'Euro)
            (list 'E 1)
            (list 'F 'Euro 'Euro 'Euro)
            (list 'G 'Token)
            (list 'H 'Box*))
  
  (set! net (make-cookie-automaton))
  (rewrite-delete (=p-lookup net 'A))
  (assert-exception (run-petrinet! net)))

(run-tests)
