; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple well-formedness)
 (export
  specify-well-formedness)
 (import (rnrs) (racr) (siple type) (siple ast))
 
 (define specify-well-formedness
   (lambda ()
     (with-specification
      siple-specification
      
      (ag-rule
       local-correct?
       (CompilationUnit
        0
        (lambda (n)
          (let ((main-decl (att-value 'main-procedure n)))
            (and
             main-decl
             (ast-subtype? main-decl 'ProcedureDeclaration)
             (= (ast-num-children (ast-child 2 main-decl)) 0)
             (type-undefined? (ast-child 3 main-decl))))))
       
       (Statement
        0
        (lambda (n)
          #t))
       
       (ProcedureDeclaration
        0
        (lambda (n)
          (and
           (or
            (type-undefined? (ast-child 3 n))
            (let ((statement-list (ast-child 1 (ast-child 4 n))))
              (and
               (> (ast-num-children statement-list) 0)
               (ast-subtype? (ast-child (ast-num-children statement-list) statement-list) 'ProcedureReturn))))
           (<= (length (att-value 'lookup n (ast-child 1 n))) 1))))
       
       (VariableDeclaration
        0
        (lambda (n)
          (<= (length (att-value 'lookup n (ast-child 1 n))) 1)))
       
       (If
        0
        (lambda (n)
          (type-boolean? (att-value 'type (ast-child 1 n)))))
       
       (While
        0
        (lambda (n)
          (type-boolean? (att-value 'type (ast-child 1 n)))))
       
       (VariableAssignment
        0
        (lambda (n)
          (not (type-error-type? (att-value 'type n)))))
       
       (ProcedureReturn
        0
        (lambda (n)
          (not (type-error-type? (att-value 'type n)))))
       
       (Write
        0
        (lambda (n)
          (not (type-error-type? (att-value 'type n)))))
       
       (Read
        0
        (lambda (n)
          (not (type-error-type? (att-value 'type n)))))
       
       (Expression
        0
        (lambda (n)
          (not (type-error-type? (att-value 'type n))))))))))