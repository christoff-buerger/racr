; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (atomic-petrinets main)
 (export)
 (import (rnrs) (racr core)
         (atomic-petrinets ast-scheme)
         (atomic-petrinets user-interface)
         (atomic-petrinets name-analysis)
         (atomic-petrinets well-formedness-analysis)
         (atomic-petrinets enabled-analysis))
 
 (when (= (specification->phase pn) 1)
   (specify-ast)
   (compile-ast-specifications pn 'AtomicPetrinet)
   (specify-name-analysis)
   (specify-well-formedness-analysis)
   (specify-enabled-analysis)
   (compile-ag-specifications pn)))