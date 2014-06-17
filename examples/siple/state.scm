; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple state)
 (export
  make-state
  state-current-frame
  state-current-frame-set!
  state-output-port
  state-allocate
  state-access
  make-frame
  frame-procedure
  frame-closure
  frame-environment
  frame-environment-set!
  frame-return-value
  frame-return-value-set!
  make-memory-location
  memory-location-value
  memory-location-value-set!)
 (import (rnrs) (racr core))
 
 (define-record-type state
   (fields (mutable current-frame) output-port))
 
 (define state-allocate
   (lambda (state decl value)
     (let* ((env (frame-environment (state-current-frame state)))
            (entry (assq decl env)))
       (if entry
           (memory-location-value-set! (cdr entry) value)
           (frame-environment-set! (state-current-frame state) (cons (cons decl (make-memory-location value)) env))))))
 
 (define state-access
   (lambda (state decl)
     (let loop ((frame (state-current-frame state)))
       (let ((entity? (assq decl (frame-environment frame))))
         (if entity?
             (cdr entity?)
             (loop (frame-closure frame)))))))
 
 (define-record-type frame
   (fields procedure closure (mutable environment) (mutable return-value)))
 
 (define-record-type memory-location
   (fields (mutable value))))