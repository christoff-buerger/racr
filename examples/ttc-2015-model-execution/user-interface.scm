; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-model-execution user-interface)
 (export specification)
 (import (rnrs) (racr core))
 
 (define specification        (create-specification)))