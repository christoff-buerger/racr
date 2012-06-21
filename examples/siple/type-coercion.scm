; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple type-coercion)
 (export
  perform-type-coercions
  specify-type-coercion)
 (import
  (rnrs)
  (racr)
  (siple ast)
  (siple type))
 
 (define perform-type-coercions
   (lambda (n)
     (let ((coercion (att-value 'find-integer->real-coercion n)))
       (if coercion
           (let ((dummy-node (create-ast siple-specification 'Constant (list "1"))))
             (rewrite-node
              coercion
              (create-ast
               siple-specification
               'RealCoercion
               (list dummy-node)))
             (rewrite-node
              dummy-node
              coercion)
             (perform-type-coercions n))))))
 
 (define specify-type-coercion
   (lambda ()
     (with-specification
      siple-specification
      
      (define list:find-integer->real-coercion
        (lambda (l)
          (let ((found?
                 (ast-find-child
                  (lambda (i n)
                    (att-value 'find-integer->real-coercion n))
                  l)))
            (and found? (att-value 'find-integer->real-coercion found?)))))
      
      (ag-rule
       find-integer->real-coercion
       
       (CompilationUnit
        0
        (lambda (n)
          (list:find-integer->real-coercion (ast-child 'Declaration* n))))
       
       (Statement
        0
        (lambda (n)
          #f))
       
       (Block
        0
        (lambda (n)
          (list:find-integer->real-coercion (ast-child 'Statement* n))))
       
       (If
        0
        (lambda (n)
          (or (att-value 'find-integer->real-coercion (ast-child 'Condition n))
              (att-value 'find-integer->real-coercion (ast-child 'Body n))
              (list:find-integer->real-coercion (ast-child 'Alternative n)))))
       
       (While
        0
        (lambda (n)
          (or (att-value 'find-integer->real-coercion (ast-child 'Condition n))
              (att-value 'find-integer->real-coercion (ast-child 'Body n)))))
       
       (VariableAssignment
        0
        (lambda (n)
          (let ((l-type (att-value 'type (ast-child 'LHand n))))
            (cond
              ((and (type-pointer? l-type)
                    (type-real? (type-rtype l-type))
                    (type-integer? (att-value 'type (ast-child 'RHand n))))
               (ast-child 'LHand n))
              (else (att-value 'find-integer->real-coercion (ast-child 'RHand n)))))))
       
       (ProcedureReturn
        0
        (lambda (n)
          (list:find-integer->real-coercion (ast-child 'Expression* n))))
       
       (Write
        0
        (lambda (n)
          (att-value 'find-integer->real-coercion (ast-child 'Expression n))))
       
       (Read
        0
        (lambda (n)
          (att-value 'find-integer->real-coercion (ast-child 'Expression n))))
       
       (ProcedureDeclaration
        0
        (lambda (n)
          (att-value 'find-integer->real-coercion (ast-child 'Body n))))
       
       (Expression
        0
        (lambda (n)
          #f))
       
       (ProcedureCall
        0
        (lambda (n)
          (or (att-value 'find-integer->real-coercion (ast-child 'Procedure n))
              (list:find-integer->real-coercion (ast-child 'Arguments n)))))
       
       (UnaryExpression
        0
        (lambda (n)
          (att-value 'find-integer->real-coercion (ast-child 'Operand n))))
       
       (BinaryExpression
        0
        (lambda (n)
          (cond
            ((and (type-integer? (att-value 'type (ast-child 'Operand1 n)))
                  (type-real? (att-value 'type (ast-child 'Operand2 n))))
             (ast-child 'Operand1 n))
            ((and (type-real? (att-value 'type (ast-child 'Operand1 n)))
                  (type-integer? (att-value 'type (ast-child 'Operand2 n))))
             (ast-child 'Operand2 n))
            (else
             (or (att-value 'find-integer->real-coercion (ast-child 'Operand1 n))
                 (att-value 'find-integer->real-coercion (ast-child 'Operand2 n))))))))))))