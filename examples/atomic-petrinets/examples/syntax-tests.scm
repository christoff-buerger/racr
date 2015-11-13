; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr testing) (atomic-petrinets user-interface))

(define (run-error-cases)
  (assert-exception ; Non-unique places.
   (petrinet: ((A) (A))))
  (assert-exception ; Non-unique transitions.
   (petrinet: ((A))
              (transition: a () ())
              (transition: a () ())))
  (assert-exception ; Unknown source place.
   (petrinet: ()
              (transition: a ((A)    ) (       ))))
  (assert-exception ; Unknown target place.
   (petrinet: ()
              (transition: a (       ) ((A)    ))))
  (assert-exception ; Non-unique ingoing arcs.
   (petrinet: ((A))
              (transition: a ((A) (A)) (       ))))
  (assert-exception ; Non-unique outgoing arcs.
   (petrinet: ((A))
              (transition: a (       ) ((A) (A))))))

(define (run-correct-cases)
  (petrinet: (         )                                                          )  ; Empty net.
  (petrinet: ((A) (B 1))                                                          )  ; No transitions.
  (petrinet: ((A) (B 1)) (transition: a (                         ) (           )))  ; No arcs.
  (petrinet: ((A) (B 1)) (transition: a ((A) (B (a 1))            ) (           )))  ; No outgoing arcs.
  (petrinet: ((A) (B 1)) (transition: a ((A) (B (a 1) (b 2) (c 3))) ((A) (B 1 b))))  ; Weights & colours.
  (petrinet: ((A) (B 1)) (transition: a (                         ) ((A) (B 1  ))))) ; No ingoing arcs.
  
(define (run-tests)
  (run-error-cases)
  (run-correct-cases))

(initialise-petrinet-language)
(run-tests)