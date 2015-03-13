; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (tinycpp-racr ast)
 (export specify-ast)
 (import (rnrs) (racr core))
 
 (define specify-ast
   (lambda (specification)
     (with-specification
      specification
      
      (ast-rule 'CompilationUnit->Declaration*<Body-sourcefile)
      (ast-rule 'Declaration->name-globalindex)
      (ast-rule 'FieldDeclaration:Declaration->)
      (ast-rule 'ClassDeclaration:Declaration->)
      (ast-rule 'ClassDefinition:ClassDeclaration->Declaration*<Body)
      (ast-rule 'WovenClassDefinition:ClassDefinition->)
      (ast-rule 'MethodDeclaration:Declaration->FieldDeclaration*<Parameters-VariableAssignment*<Body)
      (ast-rule 'Constructor:MethodDeclaration->)
      (ast-rule 'VariableAssignment->Reference<LHand-Reference<RHand)
      (ast-rule 'Reference->name-globalindex)))))