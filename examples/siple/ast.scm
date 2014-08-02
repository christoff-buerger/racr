; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple ast)
 (export
  specify-ast)
 (import (rnrs) (racr core))
 
 (define specify-ast
   (lambda (siple-specification)
     (with-specification
      siple-specification
      
      (ast-rule 'CompilationUnit->Declaration*)
      
      (ast-rule 'Statement->)
      
      (ast-rule 'Block:Statement->Statement*)
      (ast-rule 'If:Statement->Expression<Condition-Block<Body-Block*<Alternative)
      (ast-rule 'While:Statement->Expression<Condition-Block<Body)
      (ast-rule 'VariableAssignment:Statement->Expression<LHand-Expression<RHand)
      (ast-rule 'ProcedureReturn:Statement->Expression*)
      (ast-rule 'Write:Statement->Expression)
      (ast-rule 'Read:Statement->Expression)
      (ast-rule 'Assertion:Statement->Expression)
      
      (ast-rule 'Declaration:Statement->name)
      (ast-rule 'ProcedureDeclaration:Declaration->VariableDeclaration*<Parameters-returntype-Block<Body)
      (ast-rule 'VariableDeclaration:Declaration->declaredtype)
      
      (ast-rule 'Expression:Statement->)
      
      (ast-rule 'Constant:Expression->lexem)
      (ast-rule 'Reference:Expression->name)
      (ast-rule 'ProcedureCall:Expression->Expression<Procedure-Expression*<Arguments)
      
      (ast-rule 'UnaryExpression:Expression->Expression<Operand)
      
      (ast-rule 'Not:UnaryExpression->)
      (ast-rule 'UMinus:UnaryExpression->)
      (ast-rule 'RealCoercion:UnaryExpression->)
      (ast-rule 'Dereference:UnaryExpression->)
      
      (ast-rule 'BinaryExpression:Expression->Expression<Operand1-Expression<Operand2)
      
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
      (ast-rule 'Division:ArithmeticExpression->)))))