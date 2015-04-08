; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-model-execution execution-semantics)
 (export
  fire-transition!
  run-petrinet!)
 (import (rnrs) (racr core) (ttc-2015-model-execution user-interface))
 
 ;;; Execution:
 
 (define run-petrinet!
   (lambda (petrinet)
     (unless (Valid? petrinet)
       (throw-petrinets-exception "Cannot run Petri Net; The given net is not well-formed."))
     (let ((enabled (Enabled? petrinet)))
       (unless (null? enabled)
         (fire-transition! (car enabled))
         (run-petrinet! petrinet)))))
 
 (define fire-transition!
   (lambda (transition)
     (unless (Enabled? transition)
       (throw-petrinets-exception "Cannot fire transition; The transition is not enabled."))
     (let* ((argument-list (map ->value (Enabled? transition))))
       (for-each rewrite-delete (Enabled? transition))
       (ast-for-each-child
        (lambda (i n)
          (for-each
           (lambda (value)
             (rewrite-add
              (->Token* (Place n))
              (make-Token value)))
           (apply (->consumers n) argument-list)))
        (->Out transition))))))