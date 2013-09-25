; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (petrinets ast)
 (export
  specify-ast)
 (import (rnrs) (racr))
 
 (define specify-ast
   (lambda (petrinet-specification)
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
      (ast-rule 'Arc->place-functionlabel)))))