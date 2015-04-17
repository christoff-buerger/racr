; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (atomic-petrinets execution)
 (export specify-execution petrinets-exception? exception: fire-transition! run-petrinet!)
 (import (rnrs) (racr core) (atomic-petrinets analyses))
 
 ;;; Exceptions:
 
 (define-condition-type petrinets-exception
   &violation
   make-petrinets-exception
   petrinets-exception?)
 
 (define (exception: message)
   (raise-continuable
    (condition
     (make-petrinets-exception)
     (make-message-condition message))))
 
 ;;; Execution:
 
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
   (let ((consumed-tokens (map ->value enabled?)))
     (for-each rewrite-delete enabled?)
     ((att-value 'executor transition) consumed-tokens)))
 
 (define (specify-execution)
   (with-specification
    pn
    (ag-rule
     executor
     (Transition
      (lambda (n)
        (define producers (map ->consumers (=out-arcs n)))
        (define destinations (map ->Token* (map =place (=out-arcs n))))
        (lambda (consumed-tokens)
          (for-each
           (lambda (f l)
             (for-each
              (lambda (value) (rewrite-add l (:Token value)))
              (apply f consumed-tokens)))
           producers
           destinations))))))))