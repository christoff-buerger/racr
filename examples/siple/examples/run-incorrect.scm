; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (siple exception-api) (siple main))

(assert
 (call/cc
  (lambda (k)
    (with-exception-handler
     (lambda (exc)
       (if (siple-exception? exc)
           (k #t)
           (raise exc)))
     (lambda ()
       (siple-interpret (car (reverse (command-line))))
       #f)))))