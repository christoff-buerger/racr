; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (ttc-2015-fuml-activity-diagrams user-interface))

(define diagram (cadr (command-line)))
(define input?
  (let ((input (caddr (command-line))))
    (if (string=? input ":false:") #f input)))
(define mode (string->number (cadddr (command-line))))
(define print-trace?
  (not (string=? (cadddr (cdr (command-line))) ":false:")))

(run-activity-diagram diagram input? mode print-trace?)