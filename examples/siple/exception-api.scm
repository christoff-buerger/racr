; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple exception-api)
 (export
  throw-siple-exception
  siple-exception?)
 (import
  (rnrs))
 
 (define-condition-type siple-exception
   &violation
   make-siple-exception
   siple-exception?)
 
 (define throw-siple-exception
   (lambda (message)
     (raise-continuable
      (condition
       (make-siple-exception)
       (make-message-condition message))))))