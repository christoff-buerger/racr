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
  (siple type))
 
 (define perform-type-coercions
   (lambda (n siple-specification)
     (let ((coercion (att-value 'find-integer->real-coercion n)))
       (if coercion
           (let ((dummy-node (create-ast siple-specification 'Constant (list "1"))))
             (rewrite-subtree
              coercion
              (create-ast
               siple-specification
               'RealCoercion
               (list dummy-node)))
             (rewrite-subtree
              dummy-node
              coercion)
             (perform-type-coercions n siple-specification))))))
 
 (define specify-type-coercion
   (lambda (siple-specification)
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
        (lambda (n)
          (list:find-integer->real-coercion (ast-child 'Declaration* n))))
       
       (Statement
        (lambda (n)
          #f))
       
       (Block
        (lambda (n)
          (list:find-integer->real-coercion (ast-child 'Statement* n))))
       
       (If
        (lambda (n)
          (or (att-value 'find-integer->real-coercion (ast-child 'Condition n))
              (att-value 'find-integer->real-coercion (ast-child 'Body n))
              (list:find-integer->real-coercion (ast-child 'Alternative n)))))
       
       (While
        (lambda (n)
          (or (att-value 'find-integer->real-coercion (ast-child 'Condition n))
              (att-value 'find-integer->real-coercion (ast-child 'Body n)))))
       
       (VariableAssignment
        (lambda (n)
          (let ((l-type (att-value 'type (ast-child 'LHand n))))
            (cond
              ((and (type-pointer? l-type)
                    (type-real? (type-rtype l-type))
                    (type-integer? (att-value 'type (ast-child 'RHand n))))
               (ast-child 'RHand n))
              (else (att-value 'find-integer->real-coercion (ast-child 'RHand n)))))))
       
       (ProcedureReturn
        (lambda (n)
          (or
           (list:find-integer->real-coercion (ast-child 'Expression* n))
           (let ((procedure-decl (att-value 'procedure-in-context n)))
             (and procedure-decl
                  (type-real? (type-rtype (att-value 'type procedure-decl)))
                  (= (ast-num-children (ast-child 'Expression* n)) 1)
                  (type-integer? (att-value 'type (ast-child 1 (ast-child 'Expression* n))))
                  (ast-child 1 (ast-child 'Expression* n)))))))
       
       (Write
        (lambda (n)
          (att-value 'find-integer->real-coercion (ast-child 'Expression n))))
       
       (Read
        (lambda (n)
          (att-value 'find-integer->real-coercion (ast-child 'Expression n))))
       
       (ProcedureDeclaration
        (lambda (n)
          (att-value 'find-integer->real-coercion (ast-child 'Body n))))
       
       (Expression
        (lambda (n)
          #f))
       
       (ProcedureCall
        (lambda (n)
          (or
           (att-value 'find-integer->real-coercion (ast-child 'Procedure n))
           (list:find-integer->real-coercion (ast-child 'Arguments n))
           (let ((procedure-type (att-value 'type (ast-child 'Procedure n))))
             (and (type-procedure? procedure-type)
                  (= (length (type-paras procedure-type)) (ast-num-children (ast-child 'Arguments n)))
                  (exists
                   (lambda (para-type arg)
                     (and (type-real? para-type) (type-integer? (att-value 'type arg)) arg))
                   (type-paras procedure-type)
                   (ast-children (ast-child 'Arguments n))))))))
       
       (UnaryExpression
        (lambda (n)
          (att-value 'find-integer->real-coercion (ast-child 'Operand n))))
       
       (BinaryExpression
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