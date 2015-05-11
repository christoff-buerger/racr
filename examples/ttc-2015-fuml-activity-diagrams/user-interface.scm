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
 
 (define (run-activity-diagram diagram-file input-file) ; Execute diagram & print trace on std-out.
   (define activity (parse-diagram diagram-file))
   (when input-file
     (for-each
      (lambda (n)
        (define variable (=v-lookup activity (->name n)))
        (unless variable (exception: "Unknown Input"))
        (unless (eq? (->initial variable) Undefined) (exception: "Unknown Input"))
        (rewrite-terminal 'initial variable (->initial n)))
      (parse-diagram-input input-file)))
   (unless (for-all (lambda (n) (not (eq? (->initial n) Undefined))) (=variables activity))
     (exception: "Missing Input"))
   (unless (=valid? activity) (exception: "Invalid Diagram"))
   (let ((net (=petrinet activity)))
     (unless (pn:=valid? net) (exception: "Invalid Diagram"))
     (trace (->name (=initial activity)))
     (pn:run-petrinet! net)
     (for-each
      (lambda (n) (trace (->name n) " = " ((=v-accessor n))))
      (=variables activity))))
 
 (pn:initialise-petrinet-language))