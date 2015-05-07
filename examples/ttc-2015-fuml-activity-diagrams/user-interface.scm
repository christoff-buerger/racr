; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-fuml-activity-diagrams user-interface)
 (export run-activity-diagram)
 (import (rnrs) (racr core) (racr testing)
         (ttc-2015-fuml-activity-diagrams language)
         (ttc-2015-fuml-activity-diagrams parser)
         (prefix (atomic-petrinets analyses) pn:)
         (prefix (atomic-petrinets user-interface) pn:))
 
 (define (run-activity-diagram diagram-file input-file trace)
   (define activity (parse-diagram diagram-file))
   (define input (if input-file (parse-diagram-input input-file) (list)))
   ;(print-ast activity (list (cons 'valid? (lambda (v) v))) (current-output-port))
   (for-each
    (lambda (n)
      (define variable (=v-lookup activity (->name n)))
      (unless variable (exception: "Unknown Input"))
      (unless (eq? (->initial variable) Undefined) (exception: "Unknown Input"))
      (rewrite-terminal 'initial variable (->initial n)))
    input)
   (unless (for-all (lambda (n) (not (eq? (->initial n) Undefined))) (=variables activity))
     (exception: "Missing Input"))
   ;(print-ast activity (list (cons 'valid? (lambda (v) v))) (current-output-port))
   (unless (=valid? activity) (exception: "Invalid Diagram"))
   (let ((net (=petrinet activity)))
     ;(print-ast net (list) (current-output-port))
     (unless (pn:=valid? net)
       (exception: "Invalid Diagram"))
     net))
 
 (pn:initialise-petrinet-language))