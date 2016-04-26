; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (rnrs mutable-pairs) (racr testing) (siple main) (siple exception-api))

(define program (cadr (command-line)))
(define incorrect? (string=? (caddr (command-line)) ":true:"))

(set-car! (command-line) program) ; BUG: Command line arguments immutable in Larceny and Racket!
(set-cdr! (command-line) (cdddr (command-line)))

(if incorrect?
    (assert-exception siple-exception? (siple-interpret program))
    (siple-interpret program))