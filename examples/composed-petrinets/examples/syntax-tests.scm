; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr testing) (composed-petrinets user-interface) (composed-petrinets analyses))

(define (run-error-cases)
  #|
  (assert-exception ; Non-unique places.
   (petrinet: ((A) (A))))
  (assert-exception ; Non-unique transitions.
   (petrinet: ((A))
              (transition: a () ())
              (transition: a () ())))
  (assert-exception ; Unknown place.
   (petrinet: ()
              (transition: a ((A)    ) (       ))))
  (assert-exception ; Non-unique ingoing arcs.
   (petrinet: ((A))
              (transition: a ((A) (A)) (       ))))
  (assert-exception ; Non-unique outgoing arcs.
   (petrinet: ((A))
              (transition: a (       ) ((A) (A)))))
|#
  ;;; Atomic Petri nets:
  
  (assert-exception ; Non-unique in-ports
   (petrinet: net (A A) () ((A))))
  (assert-exception ; Non-unique out-ports
   (petrinet: net () (A A) ((A))))
  (assert-exception ; Unknown in-port
   (petrinet: net (A) () ()))
  (assert-exception ; Unknown out-port
   (petrinet: net () (A) ()))
  (assert-exception ; Non-unique subnets (flat)
   (compose-petrinets:
    (petrinet: a () () ())
    (petrinet: a () () ())))
  
  ;;; Flat composition structure:
  
  (assert-exception ; Unknown in-port glueing
   (compose-petrinets:
    (petrinet: a (A) () ((A)))
    (petrinet: b () (A) ((A)))
    ((b A) (a A*))))
  (assert-exception ; Unknown out-port glueing
   (compose-petrinets:
    (petrinet: a (A) () ((A)))
    (petrinet: b () (A) ((A)))
    ((b A*) (a A))))
  (assert-exception ; in- to in-port glueing
   (compose-petrinets:
    (petrinet: a (A) () ((A)))
    (petrinet: b (A) () ((A)))
    ((b A) (a A))))
  (assert-exception ; out- to out-port glueing
   (compose-petrinets:
    (petrinet: a () (A) ((A)))
    (petrinet: b () (A) ((A)))
    ((b A) (a A))))
  (assert-exception ; in- to out-port glueing (wrong direction of token flow)
   (compose-petrinets:
    (petrinet: a (A) () ((A)))
    (petrinet: b () (A) ((A)))
    ((a A) (b A))))
  
  ;;; Nested composition structure:
  
  (let ((net1 ; Non-unique subnets
         (compose-petrinets:
          (petrinet: a () () ())
          (compose-petrinets:
           (petrinet: b () () ())
           (petrinet: c () () ()))))
        (net2
         (compose-petrinets:
          (petrinet: d () () ())
          (petrinet: b () () ()))))
    (assert-exception
     (compose-petrinets: net1 net2)))
  
  (let ((net1 ; Unknown in-port glueing
         (compose-petrinets: ; Non-glued: (b () (B))
          (petrinet: a (A) () ((A)))
          (compose-petrinets: ; Non-glued: (b () (A B))
           (petrinet: b (A) (A B) ((A) (B)))
           (petrinet: c () (A) ((A)))
           ((c A) (b A)))
          ((b A) (a A))))
        (net2
         (compose-petrinets: ; Non-glued: (d (B) ())
          (petrinet: d (A B) () ((A) (B)))
          (petrinet: e () (A) ((A)))
          ((e A) (d A)))))
    (assert-exception
     (compose-petrinets: net1 net2 ((b B) (d B*)))))
  
  (let ((net1 ; Unknown out-port glueing
         (compose-petrinets: ; Non-glued: (b () (B))
          (petrinet: a (A) () ((A)))
          (compose-petrinets: ; Non-glued: (b () (A B))
           (petrinet: b (A) (A B) ((A) (B)))
           (petrinet: c () (A) ((A)))
           ((c A) (b A)))
          ((b A) (a A))))
        (net2
         (compose-petrinets: ; Non-glued: (d (B) ())
          (petrinet: d (A B) () ((A) (B)))
          (petrinet: e () (A) ((A)))
          ((e A) (d A)))))
    (assert-exception
     (compose-petrinets: net1 net2 ((b B*) (d B)))))
  #t)

(define (run-correct-cases)
  #|
  (petrinet: (         )                                                          )  ; Empty net.
  (petrinet: ((A) (B 1))                                                          )  ; No transitions.
  (petrinet: ((A) (B 1)) (transition: a (                         ) (           )))  ; No arcs.
  (petrinet: ((A) (B 1)) (transition: a ((A) (B (a 1))            ) (           )))  ; No outgoing arcs.
  (petrinet: ((A) (B 1)) (transition: a ((A) (B (a 1) (b 2) (c 3))) ((A) (B 1 b))))  ; Weights & colours.
  (petrinet: ((A) (B 1)) (transition: a (                         ) ((A) (B 1  ))))  ; No ingoing arcs.
|#
  (compose-petrinets: ; Flat composition
   (petrinet: a (A) (B) ((A) (B)))
   (petrinet: b (B) (A) ((A) (B)))
   ((b A) (a A))
   ((a B) (b B)))
  (let ((net1 ; Nested composition
         (compose-petrinets: ; Non-glued: (a (B) ()), (b () (B))
          (petrinet: a (A B) () ((A) (B)))
          (compose-petrinets: ; Non-glued: (b () (A B))
           (petrinet: b (A) (A B) ((A) (B)))
           (petrinet: c () (A) ((A)))
           ((c A) (b A)))
          ((b A) (a A))))
        (net2
         (compose-petrinets: ; Non-glued: (d (B) ()), (e () (B))
          (petrinet: d (A B) () ((A) (B)))
          (petrinet: e () (A B) ((A) (B)))
          ((e A) (d A)))))
    (compose-petrinets: net1 net2 ((b B) (d B)) ((e B) (a B))))
  #t)

(define (run-tests)
  (run-error-cases)
  (run-correct-cases))

(initialise-petrinet-language)
(run-tests)