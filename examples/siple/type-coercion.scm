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
          (list:find-integer->real-coercion (ast-child 1 n))))
       
       (Statement
        0
        (lambda (n)
          #f))
       
       (Block
        0
        (lambda (n)
          (list:find-integer->real-coercion (ast-child 1 n))))
       
       (If
        0
        (lambda (n)
          (or (att-value 'find-integer->real-coercion (ast-child 1 n))
              (att-value 'find-integer->real-coercion (ast-child 2 n))
              (list:find-integer->real-coercion (ast-child 3 n)))))
       
       (While
        0
        (lambda (n)
          (or (att-value 'find-integer->real-coercion (ast-child 1 n))
              (att-value 'find-integer->real-coercion (ast-child 2 n)))))
       
       (VariableAssignment
        0
        (lambda (n)
          (let ((l-type (att-value 'type (ast-child 1 n))))
            (cond
              ((and (type-pointer? l-type)
                    (type-real? (type-rtype l-type))
                    (type-integer? (att-value 'type (ast-child 2 n))))
               (ast-child 2 n))
              (else (att-value 'find-integer->real-coercion (ast-child 2 n)))))))
       
       (ProcedureReturn
        0
        (lambda (n)
          (list:find-integer->real-coercion (ast-child 1 n))))
       
       (Write
        0
        (lambda (n)
          (att-value 'find-integer->real-coercion (ast-child 1 n))))
       
       (Read
        0
        (lambda (n)
          (att-value 'find-integer->real-coercion (ast-child 1 n))))
       
       (ProcedureDeclaration
        0
        (lambda (n)
          (att-value 'find-integer->real-coercion (ast-child 4 n))))
       
       (Expression
        0
        (lambda (n)
          #f))
       
       (ProcedureCall
        0
        (lambda (n)
          (or (att-value 'find-integer->real-coercion (ast-child 1 n))
              (list:find-integer->real-coercion (ast-child 2 n)))))
       
       (UnaryExpression
        0
        (lambda (n)
          (att-value 'find-integer->real-coercion (ast-child 1 n))))
       
       (BinaryExpression
        0
        (lambda (n)
          (cond
            ((and (type-integer? (att-value 'type (ast-child 1 n)))
                  (type-real? (att-value 'type (ast-child 2 n))))
             (ast-child 1 n))
            ((and (type-real? (att-value 'type (ast-child 1 n)))
                  (type-integer? (att-value 'type (ast-child 2 n))))
             (ast-child 2 n))
            (else
             (or (att-value 'find-integer->real-coercion (ast-child 1 n))
                 (att-value 'find-integer->real-coercion (ast-child 2 n))))))))))))