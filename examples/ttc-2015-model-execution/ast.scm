; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (ttc-2015-model-execution ast)
 (export
  specify-ast)
 (import (rnrs) (racr core) (ttc-2015-model-execution user-interface))
 
 (define (specify-ast)
   (with-specification
    pn
    
    (ast-rule 'AtomicPetrinet->Place*-Transition*)
    (ast-rule 'Place->name-Token*)
    (ast-rule 'Token->value)
    (ast-rule 'Transition->name-Arc*<In-Arc*<Out)
    (ast-rule 'Arc->place-consumers))))