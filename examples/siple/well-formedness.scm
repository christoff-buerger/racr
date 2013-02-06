; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple well-formedness)
 (export
  specify-well-formedness)
 (import (rnrs) (racr) (siple type))
 
 (define specify-well-formedness
   (lambda (siple-specification)
     (with-specification
      siple-specification
      
      (ag-rule
       local-correct?
       
       (CompilationUnit
        (lambda (n)
          (let ((main-decl (att-value 'main-procedure n)))
            (and
             main-decl
             (ast-subtype? main-decl 'ProcedureDeclaration)
             (= (ast-num-children (ast-child 'Parameters main-decl)) 0)
             (type-undefined? (ast-child 'returntype main-decl))))))
       
       (Statement
        (lambda (n)
          #t))
       
       ; Simple version not considering programs, that are still valid regarding procedure termination
       ; although the last statement is not a procedure return. E.g., the following procedure is correct:
       ;
       ; Procedure test(Var j:Integer):Integer Begin
       ;    If j = 1 Then
       ;       Return 1;
       ;    Else
       ;       If j = 2 Then
       ;          Return 2;
       ;       Else
       ;          Return 3;
       ;       Fi;
       ;    Fi;
       ; End;
       ;
       ;(ProcedureDeclaration
       ; (lambda (n)
       ;   (and
       ;    (or
       ;     (type-undefined? (ast-child 'returntype n))
       ;     (let ((statement-list (ast-child 1 (ast-child 'Body n))))
       ;       (and
       ;        (> (ast-num-children statement-list) 0)
       ;        (ast-subtype? (ast-child (ast-num-children statement-list) statement-list) 'ProcedureReturn))))
       ;    (<= (length (att-value 'lookup n (ast-child 'name n))) 1))))
       
       (ProcedureDeclaration
        (lambda (n)
          (and
           (<= (length (att-value 'lookup n (ast-child 'name n))) 1)
           (or
            (type-undefined? (ast-child 'returntype n))
            (and
             (not (null? (att-value 'cf-local-exits (ast-child 'Body n))))
             (for-all
               (lambda (n)
                 (att-value 'procedure-return-in-context n))
               (att-value 'cf-local-exits (ast-child 'Body n))))))))
       
       (VariableDeclaration
        (lambda (n)
          (<= (length (att-value 'lookup n (ast-child 'name n))) 1)))
       
       (If
        (lambda (n)
          (type-boolean? (att-value 'type (ast-child 'Condition n)))))
       
       (While
        (lambda (n)
          (type-boolean? (att-value 'type (ast-child 'Condition n)))))
       
       (VariableAssignment
        (lambda (n)
          (not (type-error-type? (att-value 'type n)))))
       
       (ProcedureReturn
        (lambda (n)
          (not (type-error-type? (att-value 'type n)))))
       
       (Write
        (lambda (n)
          (not (type-error-type? (att-value 'type n)))))
       
       (Read
        (lambda (n)
          (not (type-error-type? (att-value 'type n)))))
       
       (Expression
        (lambda (n)
          (not (type-error-type? (att-value 'type n))))))))))