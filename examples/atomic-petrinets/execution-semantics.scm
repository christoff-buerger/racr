; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (atomic-petrinets execution-semantics)
 (export specify-execution-semantics fire-transition! run-petrinet!)
 (import (rnrs) (racr core) (atomic-petrinets query-support))
 
 (define (run-petrinet! petrinet)
   (unless (=valid? petrinet)
     (exception: "Cannot run Petri Net; The given net is not well-formed."))
   (let ((enabled? (find =enabled? (=transitions petrinet))))
     (when enabled?
       (fire-transition! enabled?)
       (run-petrinet! petrinet))))
 
 (define (fire-transition! transition)
   (define enabled? (=enabled? transition))
   (unless enabled?
     (exception: "Cannot fire transition; The transition is not enabled."))
   (let ((arguments (map ->value enabled?)))
     (for-each rewrite-delete enabled?)
     ((=executor transition) arguments)))
 
 (define (specify-execution-semantics)
   (with-specification
    pn
    
    (ag-rule
     executor
     (Transition
      (lambda (n)
        (define out-arcs (->* (->Out n)))
        (define producers (map ->consumers out-arcs))
        (define contribution-token-lists (map ->Token* (map =place out-arcs)))
        (lambda (arguments)
          (for-each
           (lambda (f l)
             (for-each
              (lambda (value) (rewrite-add l (:Token value)))
              (apply f arguments)))
           producers
           contribution-token-lists))))))))