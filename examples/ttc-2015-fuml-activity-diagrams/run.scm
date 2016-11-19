; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (ttc-2015-fuml-activity-diagrams user-interface))

(define arguments    (command-line))
(define diagram      (list-ref arguments 1))
(define input?       (let ((input (list-ref arguments 2)))
                       (if (string=? input ":false:") #f input)))
(define mode         (string->number (list-ref arguments 3)))
(define print-trace? (not (string=? (list-ref arguments 4) ":false:")))

(run-activity-diagram diagram input? mode print-trace?)