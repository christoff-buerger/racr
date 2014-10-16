; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (racr testing) (tinycpp-racr main) (tinycpp-racr exception-api))

(assert-exception
 tinycpp-racr-exception?
 (apply compile (cdr (command-line))))