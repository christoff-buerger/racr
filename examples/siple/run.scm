; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (rnrs mutable-pairs) (racr testing) (siple main) (siple exception-api))

(define arguments  (command-line))
(define program    (list-ref arguments 1))
(define incorrect? (string=? (list-ref arguments 2) ":true:"))

(set-car! (command-line) program) ; BUG: Command line arguments immutable in Larceny and Racket!
(set-cdr! (command-line) (cdddr (command-line)))

(if incorrect?
    (assert-exception siple-exception? (siple-interpret program))
    (siple-interpret program))