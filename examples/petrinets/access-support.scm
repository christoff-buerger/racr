; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (petrinets access-support)
 (export
  specify-access-support)
 (import (rnrs) (racr core))
 
 (define specify-access-support
   (lambda (petrinet-specification)
     (with-specification
      petrinet-specification
      
      (ag-rule
       containing-petrinet
       ((AtomicPetrinet Place*)
        (lambda (n)
          (ast-parent (ast-parent n)))))))))