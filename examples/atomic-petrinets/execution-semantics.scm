; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (atomic-petrinets execution-semantics)
 (export fire-transition! run-petrinet!)
 (import (rnrs) (racr core) (atomic-petrinets query-support))
 
 (define (run-petrinet! petrinet)
   (unless (=valid? petrinet)
     (throw-petrinets-exception "Cannot run Petri Net; The given net is not well-formed."))
   (let ((enabled (=enabled? petrinet)))
     (unless (null? enabled)
       (fire-transition! (car enabled))
       (run-petrinet! petrinet))))
 
 (define (fire-transition! transition)
   (unless (=enabled? transition)
     (throw-petrinets-exception "Cannot fire transition; The transition is not enabled."))
   (let* ((argument-list (map ->value (=enabled? transition))))
     (for-each rewrite-delete (=enabled? transition))
     (ast-for-each-child
      (lambda (i n)
        (for-each
         (lambda (value)
           (rewrite-add
            (->Token* (=place n))
            (:Token value)))
         (apply (->consumers n) argument-list)))
      (->Out transition)))))