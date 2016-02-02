; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr testing) (siple main) (siple exception-api))

(define program (cadr (command-line)))
(define incorrect? (string=? (caddr (command-line)) ":true:"))

(if incorrect?
    (assert-exception siple-exception? (siple-interpret (cadr (command-line))))
    (siple-interpret (cadr (command-line))))