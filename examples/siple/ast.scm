; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple ast)
 (export
  siple-specification
 specify-ast)
 (import (rnrs) (racr))
 
 (define siple-specification (create-specification))
 
 (define specify-ast
   (lambda ()
 (with-specification
  siple-specification
  
  (ast-rule 'CompilationUnit->Declaration*)
  
  (ast-rule 'Statement->)
  
  (ast-rule 'Block:Statement->Statement*)
  (ast-rule 'If:Statement->Expression-Block-Block*)
  (ast-rule 'While:Statement->Expression-Block)
  (ast-rule 'VariableAssignment:Statement->Expression-Expression)
  (ast-rule 'ProcedureReturn:Statement->Expression*)
  (ast-rule 'Write:Statement->Expression)
  (ast-rule 'Read:Statement->Expression)
  
  (ast-rule 'Declaration:Statement->name)
  (ast-rule 'ProcedureDeclaration:Declaration->VariableDeclaration*-returnType-Block)
  (ast-rule 'VariableDeclaration:Declaration->declaredType)
  
  (ast-rule 'Expression:Statement->)
  
  (ast-rule 'Constant:Expression->lexem)
  (ast-rule 'Reference:Expression->name)
  (ast-rule 'ProcedureCall:Expression->Expression-Expression*)
  
  (ast-rule 'UnaryExpression:Expression->Expression)
  
  (ast-rule 'Not:UnaryExpression->)
  (ast-rule 'UMinus:UnaryExpression->)
  (ast-rule 'RealCoercion:UnaryExpression->)
  (ast-rule 'Dereference:UnaryExpression->)
  
  (ast-rule 'BinaryExpression:Expression->Expression-Expression)
  
  (ast-rule 'LogicExpression:BinaryExpression->)
  
  (ast-rule 'And:LogicExpression->)
  (ast-rule 'Or:LogicExpression->)
  
  (ast-rule 'EqualityExpression:BinaryExpression->)
  
  (ast-rule 'Equal:EqualityExpression->)
  (ast-rule 'GreaterThan:EqualityExpression->)
  (ast-rule 'LesserThan:EqualityExpression->)
  (ast-rule 'GreaterThanEqual:EqualityExpression->)
  (ast-rule 'LesserThanEqual:EqualityExpression->)
  
  (ast-rule 'ArithmeticExpression:BinaryExpression->)
  
  (ast-rule 'Addition:ArithmeticExpression->)
  (ast-rule 'Subtraction:ArithmeticExpression->)
  (ast-rule 'Multiplication:ArithmeticExpression->)
  (ast-rule 'Division:ArithmeticExpression->)
  
  (compile-ast-specifications 'CompilationUnit)))))