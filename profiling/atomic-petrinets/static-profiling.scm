; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (atomic-petrinets static-profiling)
 (export profile-static)
 (import (rnrs) (for (atomic-petrinets profiling) expand run))
 
 (define (profile-static $executions)
   profiling-net
   ;(profile $executions)
   ))