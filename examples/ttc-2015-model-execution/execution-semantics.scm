; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-model-execution execution-semantics)
 (export
  petrinets-exception?
  fire-transition!
  run-petrinet!)
 (import (rnrs) (racr core) (ttc-2015-model-execution user-interface))
 
 ;;; Exceptions:
 
 (define-condition-type petrinets-exception &violation make-petrinets-exception petrinets-exception?)
 
 (define throw-petrinets-exception
   (lambda (message)
     (raise-continuable
      (condition
       (make-petrinets-exception)
       (make-message-condition message)))))
 
 ;;; Execution:
 
 (define run-petrinet!
   (lambda (petrinet)
     (unless (att-value 'well-formed? petrinet)
       (throw-petrinets-exception "Cannot run Petri Net; The given net is not well-formed."))
     (let ((enabled (att-value 'enabled? petrinet)))
       (unless (null? enabled)
         (fire-transition! (car enabled))
         (run-petrinet! petrinet)))))
 
 (define fire-transition!
   (lambda (transition)
     (unless (att-value 'enabled? transition)
       (throw-petrinets-exception "Cannot fire transition; The transition is not enabled."))
     (let* ((argument-list
             (map
              (lambda (token)
                (ast-child 'value token))
              (att-value 'enabled? transition))))
       (for-each
        rewrite-delete
        (att-value 'enabled? transition))
       (ast-for-each-child
        (lambda (i out)
          (for-each
           (lambda (new-token)
             (rewrite-add
              (ast-child 'Token* (att-value 'place out))
              (create-ast
               specification
               'Token
               (list
                new-token))))
           (apply
            (ast-child 'functionlabel out)
            argument-list)))
        (ast-child 'Out transition))))))