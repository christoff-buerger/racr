; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (atomic-petrinets profiling))

(define arguments     (command-line))
(define $transitions  (string->number (list-ref arguments 1)))
(define $influenced   (string->number (list-ref arguments 2)))
(define $local-places (string->number (list-ref arguments 3)))
(define $local-tokens (string->number (list-ref arguments 4)))
(define $executions   (string->number (list-ref arguments 5)))

(profile-net
 (make-profiling-net $transitions $influenced $local-places $local-tokens)
 $executions)
