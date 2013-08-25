; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (petrinets ast)
 (export
  specify-ast
  petrinet-specification)
 (import (rnrs) (racr))
 
 (define petrinet-specification (create-specification))
 
 (define specify-ast
   (lambda ()
     (with-specification
      petrinet-specification
      (ast-rule 'Petrinet->issubnet)
      (ast-rule 'AtomicPetrinet:Petrinet->name-Place*-Transition*-Port*)
      (ast-rule 'ComposedPetrinet:Petrinet->Petrinet<Net1-Petrinet<Net2-Glueing*)
      (ast-rule 'Glueing->outport-inport)
      (ast-rule 'Port->place)
      (ast-rule 'InPort:Port->)
      (ast-rule 'OutPort:Port->)
      (ast-rule 'Place->name-Token*)
      (ast-rule 'Token->value)
      (ast-rule 'Transition->name-Arc*<In-Arc*<Out)
      (ast-rule 'Arc->place-functionlabel)
      (compile-ast-specifications 'Petrinet)))))