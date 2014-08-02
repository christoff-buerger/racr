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
  (racr core)
  (siple type))
 
 (define perform-type-coercions
   (lambda (n siple-specification)
     (define perform-type-coercion
       (lambda (n)
         (let ((dummy-node (create-ast-bud)))
           (rewrite-subtree
            dummy-node
            (create-ast
             siple-specification
             'RealCoercion
             (list (rewrite-subtree n dummy-node)))))))
     (perform-rewrites
      n
      'bottom-up
      (lambda (n)
        (let ((coercion? (att-value 'has-local-integer->real-coercion? n)))
          (if coercion? (perform-type-coercion coercion?) #f))))))
 
 (define specify-type-coercion
   (lambda (siple-specification)
     (with-specification
      siple-specification
      
      (ag-rule
       has-local-integer->real-coercion?
       
       (CompilationUnit
        (lambda (n)
          #f))
       
       (Statement
        (lambda (n)
          #f))
       
       (VariableAssignment
        (lambda (n)
          (let ((l-type (att-value 'type (ast-child 'LHand n))))
            (and (type-pointer? l-type)
                 (type-real? (type-rtype l-type))
                 (type-integer? (att-value 'type (ast-child 'RHand n)))
                 (ast-child 'RHand n)))))
       
       (ProcedureReturn
        (lambda (n)
          (let ((procedure-decl (att-value 'procedure-in-context n)))
            (and procedure-decl
                 (type-real? (type-rtype (att-value 'type procedure-decl)))
                 (= (ast-num-children (ast-child 'Expression* n)) 1)
                 (type-integer? (att-value 'type (ast-child 1 (ast-child 'Expression* n))))
                 (ast-child 1 (ast-child 'Expression* n))))))
       
       (ProcedureCall
        (lambda (n)
          (let ((procedure-type (att-value 'type (ast-child 'Procedure n))))
            (and (type-procedure? procedure-type)
                 (= (length (type-paras procedure-type)) (ast-num-children (ast-child 'Arguments n)))
                 (exists
                  (lambda (para-type arg)
                    (and (type-real? para-type) (type-integer? (att-value 'type arg)) arg))
                  (type-paras procedure-type)
                  (ast-children (ast-child 'Arguments n)))))))
       
       (BinaryExpression
        (lambda (n)
          (or
           (and (type-integer? (att-value 'type (ast-child 'Operand1 n)))
                (type-real? (att-value 'type (ast-child 'Operand2 n)))
                (ast-child 'Operand1 n))
           (and (type-real? (att-value 'type (ast-child 'Operand1 n)))
                (type-integer? (att-value 'type (ast-child 'Operand2 n)))
                (ast-child 'Operand2 n))))))))))