; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr testing) (composed-petrinets user-interface) (composed-petrinets analyses))

(define (make-net-1)
  (compose-petrinets: ; Not glued: (in: c | out: c | in/out: a)
   (petrinet: a (in* in/out) (in/out) ((in*) (in/out)))
   (compose-petrinets:
    (petrinet: b (in/out*) (in/out*) ((in/out*)))
    (petrinet: c (in) (out* out) ((out*) (in) (out)))
    ((c out*) (b in/out*)))
   ((b in/out*) (a in*))))

(define (make-net-2)
  (compose-petrinets: ; Not glued: (in: f | out: d | in/out: e)
   (compose-petrinets:
    (petrinet: d (in*) (out) ((in*) (out)))
    (petrinet: e (in/out* in/out) (in/out* in/out) ((in/out*) (in/out)))
    ((e in/out*) (d in*)))
   (petrinet: f (in) (out*) ((in) (out*)))
   ((f out*) (e in/out*))))

(define (run-error-cases)
  ;;; Atomic Petri nets:
  
  (assert-exception ; Non-unique in-ports
   (petrinet: net (A A) () ((A))))
  (assert-exception ; Non-unique out-ports
   (petrinet: net () (A A) ((A))))
  (assert-exception ; Unknown in-port
   (petrinet: net (A) () ()))
  (assert-exception ; Unknown out-port
   (petrinet: net () (A) ()))
  
  ;;; Flat composition structure:
  
  (assert-exception ; Non-unique subnets
   (compose-petrinets:
    (petrinet: a () () ())
    (petrinet: a () () ())))
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
  (assert-exception ; Twisted glueing (in-port as out-port and vice versa)
   (compose-petrinets:
    (petrinet: a (A) () ((A)))
    (petrinet: b () (A) ((A)))
    ((a A) (b A))))
  (assert-exception ; Fusion of places of the same atomic net
   (compose-petrinets:
    (petrinet: a (A) (A) ((A)))
    (petrinet: b (A) (B) ((A) (B)))
    ((a A) (b A))
    ((b B) (a A))))
  
  ;;; Nested composition structure:
  
  (let ((net-1 ; Non-unique subnets
         (compose-petrinets:
          (petrinet: a () () ())
          (compose-petrinets:
           (petrinet: b () () ())
           (petrinet: c () () ()))))
        (net-2
         (compose-petrinets:
          (petrinet: d () () ())
          (petrinet: b () () ()))))
    (assert-exception
     (compose-petrinets: net-1 net-2)))
  
  (assert-exception ; Unknown in-port glueing
   (compose-petrinets: (make-net-1) (make-net-2) ((c out) (f /in/))))
  (assert-exception ; Unknown out-port glueing
   (compose-petrinets: (make-net-1) (make-net-2) ((d /out/) (c in))))
  (assert-exception ; in- to in-port glueing
   (compose-petrinets: (make-net-1) (make-net-2) ((c in) (f in))))
  (assert-exception ; out- to out-port glueing
   (compose-petrinets: (make-net-1) (make-net-2) ((d out) (c out))))
  (assert-exception ; Twisted glueing (in-port as out-port and vice versa)
   (compose-petrinets: (make-net-1) (make-net-2) ((f in) (c out))))
  (assert-exception ; Fusion of places of the same atomic net
   (compose-petrinets: (make-net-1) (make-net-2)
                       ((c out) (e in/out))
                       ((e in/out) (a in/out))
                       ((a in/out) (c in))))
  (assert-exception ; Glueing of shadowed in-port
   (compose-petrinets: (make-net-1) (make-net-2)
                       ((a in/out) (e in/out*))))
  (assert-exception ; Glueing of shadowed out-port
   (compose-petrinets: (make-net-1) (make-net-2)
                       ((c out*) (f in)))))

(define (run-correct-cases)
  (make-net-1)
  (make-net-2)
  (compose-petrinets: (make-net-1) (make-net-2)
                      ((d out) (a in/out))
                      ((a in/out) (e in/out))
                      ((e in/out) (c in))
                      ((c out) (f in))))

(define (run-tests)
  (run-error-cases)
  (run-correct-cases))

(initialise-petrinet-language)
(run-tests)