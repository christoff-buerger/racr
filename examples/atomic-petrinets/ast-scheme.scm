; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (atomic-petrinets ast-scheme)
 (export specify-ast)
 (import (rnrs) (racr core) (atomic-petrinets query-support))
 
 (define (specify-ast)
   (with-specification
    pn
    (ast-rule 'AtomicPetrinet->Place*-Transition*)
    (ast-rule 'Place->name-Token*)
    (ast-rule 'Token->value)
    (ast-rule 'Transition->name-Arc*<In-Arc*<Out)
    (ast-rule 'Arc->place-consumers))))