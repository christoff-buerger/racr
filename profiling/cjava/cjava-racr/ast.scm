; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: M. Tasić

#!r6rs

(library
 (cjava-racr ast)
 (export specify-ast)
 (import (rnrs) (racr core))
 
 (define specify-ast
   (lambda (specification)
     (with-specification
      specification
      
      (ast-rule 'CompilationUnit->ClassDeclaration*<Body-CompositionProgram)
      
      (ast-rule 'CompositionProgram->BindComposer*<Composers)
      (ast-rule 'BindComposer->targetname-sourcename)
      
      (ast-rule 'Statement->)
      (ast-rule 'Declaration:Statement->name)
      (ast-rule 'ClassDeclaration:Declaration->Declaration*<Body-srcfile)
      (ast-rule 'MethodDeclaration:Declaration->FieldDeclaration*<Parameters-Statement*<Body) ; Ensured by parser: Body consists only of field declarations and assignments.
      (ast-rule 'FieldDeclaration:Declaration->)      
      (ast-rule 'DeclarationHook:Declaration->)
      (ast-rule 'VariableAssignment:Statement->Reference<LHand-Reference<RHand)
      (ast-rule 'Reference:Statement->name)))))