; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (petrinets ast)
 (export
  specify-ast
  petrinet-spec)
 (import (rnrs) (racr))
 
 (define petrinet-spec (create-specification))
 
 (define specify-ast
   (lambda ()
     (with-specification
      petrinet-spec
      (ast-rule 'Petrinet->Place*-Transition*)
      (ast-rule 'Place->name-Token*)
      (ast-rule 'Token->value)
      (ast-rule 'Transition->name-Arc*<In-Arc*<Out)
      (ast-rule 'Arc->place-functionlabel)
      (compile-ast-specifications 'Petrinet)))))