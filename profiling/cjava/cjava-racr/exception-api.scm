; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: M. Tasić

#!r6rs

(library
 (cjava-racr exception-api)
 (export
  throw-cjava-racr-exception
  cjava-racr-exception?)
 (import
  (rnrs))
 
 (define-condition-type cjava-racr-exception
   &violation
   make-cjava-racr-exception
   cjava-racr-exception?)
 
 (define throw-cjava-racr-exception
   (lambda (message)
     (raise-continuable
      (condition
       (make-cjava-racr-exception)
       (make-message-condition message))))))