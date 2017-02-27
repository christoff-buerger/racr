; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple well-formedness)
 (export
  specify-well-formedness)
 (import (rnrs) (racr core) (siple type))
 
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
       
       ; A procedure declaration is well-formed, if (1) it is no redeclaration, (2) its
       ; parameters have unique names and (3) its last executed statement always is a
       ; return statement if it has a return type.
       (ProcedureDeclaration
        (lambda (n)
          (and
           (eq? (att-value 'lookup n (ast-child 'name n)) n) ; (1)
           (not ; (2)
            (ast-find-child
             (lambda (i par1)
               (not
                (eq?
                 par1
                 (ast-find-child
                  (lambda (i par2)
                    (string=? (ast-child 'name par2) (ast-child 'name par1)))
                  (ast-child 'Parameters n)))))
             (ast-child 'Parameters n)))
           (or ; (3)
            (type-undefined? (ast-child 'returntype n))
            (and
             (not (null? (att-value 'cf-local-exits (ast-child 'Body n))))
             (for-all
                 (lambda (n)
                   (att-value 'procedure-return-in-context n))
               (att-value 'cf-local-exits (ast-child 'Body n))))))))
       
       ; A variable declaration (except parameter declarations, which are checked by
       ; their respective procedure declaration) is well formed, if it is no redeclaration.
       (VariableDeclaration
        (lambda (n)
          (eq? (att-value 'lookup n (ast-child 'name n)) n)))
       
       (If
        (lambda (n)
          (type-boolean? (att-value 'type (ast-child 'Condition n)))))
       
       (While
        (lambda (n)
          (type-boolean? (att-value 'type (ast-child 'Condition n)))))
       
       (VariableAssignment
        (lambda (n)
          (let ((l-type (att-value 'type (ast-child 'LHand n))))
            (and
             (type-pointer? l-type)
             (type-beq? (type-rtype l-type) (att-value 'type (ast-child 'RHand n)))))))
       
       (ProcedureReturn
        (lambda (n)
          (let* ((procedure-decl (att-value 'procedure-in-context n))
                 (procedure-rtype (and procedure-decl (type-rtype (att-value 'type procedure-decl)))))
            (and
             procedure-rtype
             (not (type-error-type? procedure-rtype))
             (if (> (ast-num-children (ast-child 'Expression* n)) 0)
                 (type-beq? (att-value 'type (ast-child 1 (ast-child 'Expression* n))) procedure-rtype)
                 (type-undefined? procedure-rtype))))))
       
       (Write
        (lambda (n)
          (let ((r-type (att-value 'type (ast-child 'Expression n))))
            (not
             (or
              (type-pointer? r-type)
              (type-procedure? r-type)
              (type-undefined? r-type))))))
       
       (Read
        (lambda (n)
          (let* ((type (att-value 'type (ast-child 'Expression n)))
                 (rtype (type-rtype type)))
            (and
             (type-pointer? type)
             (not
              (or
               (type-pointer? rtype)
               (type-procedure? rtype)
               (type-undefined? rtype)))))))
       
       (Assertion
        (lambda (n)
          (type-boolean? (att-value 'type (ast-child 'Expression n)))))
       
       (Expression
        (lambda (n)
          (not (type-error-type? (att-value 'type n))))))))))