; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (tinycpp-racr exception-api)
 (export
  throw-tinycpp-racr-exception
  tinycpp-racr-exception?)
 (import
  (rnrs))
 
 (define-condition-type tinycpp-racr-exception
   &violation
   make-tinycpp-racr-exception
   tinycpp-racr-exception?)
 
 (define throw-tinycpp-racr-exception
   (lambda (message)
     (raise-continuable
      (condition
       (make-tinycpp-racr-exception)
       (make-message-condition message))))))