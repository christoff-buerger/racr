; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(import (rnrs) (prefix (tinycpp-racr main) tcpp:))

(apply tcpp:compile (cdr (command-line)))