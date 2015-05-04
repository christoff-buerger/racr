; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-fuml-activity-diagrams user-interface)
 (export run-activity-diagram)
 (import (rnrs) (racr core) (racr testing)
         (ttc-2015-fuml-activity-diagrams language)
         (ttc-2015-fuml-activity-diagrams parser))
 
 (define (run-activity-diagram diagram-file input-file trace)
   (define activity (parse-diagram diagram-file))
   (define input (if input-file (parse-diagram-input input-file) (list)))
   ;(print-ast activity (list (cons 'valid? (lambda (v) v))) (current-output-port))
   (for-each
    (lambda (n)
      (define variable (=var activity (->name n)))
      (unless variable (exception: "Unknown Input"))
      (unless (eq? (->initial variable) Undefined) (exception: "Unknown Input"))
      (rewrite-terminal 'initial variable (->initial n)))
    input)
   ;(print-ast activity (list (cons 'valid? (lambda (v) v))) (current-output-port))
   (unless (=valid? activity) (exception: "Invalid Diagram"))))