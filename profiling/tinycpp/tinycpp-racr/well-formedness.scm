; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (tinycpp-racr well-formedness)
 (export
  specify-well-formedness)
 (import (rnrs) (racr core) (tinycpp-racr support-api))
 
 (define specify-well-formedness
   (lambda (specification)
     (with-specification
      specification
      
      (ag-rule
       correct?
       
       (CompilationUnit
        (lambda (n)
          (not
           (ast-find-child
            (lambda (i n)
              (not (att-value 'correct? n)))
            (ast-child 'Body n)))))
       
       (Declaration
        (lambda (n)
          (att-value 'local-correct? n)))
       
       (ClassDefinition
        (lambda (n)
          (or
           (pair? (ast-child 'name n))
           (and
            (att-value 'local-correct? n)
            (not
             (ast-find-child
              (lambda (i n)
                (not (att-value 'correct? n)))
              (ast-child 'Body n)))))))
       
       (MethodDeclaration
        (lambda (n)
          (and
           (att-value 'local-correct? n)
           (not (ast-find-child
                 (lambda (i n)
                   (not (att-value 'correct? n)))
                 (ast-child 'Parameters n)))
           (not (ast-find-child
                 (lambda (i n)
                   (not (att-value 'correct? n)))
                 (ast-child 'Body n))))))
       
       (VariableAssignment
        (lambda (n)
          (and
           (att-value 'correct? (ast-child 'LHand n))
           (att-value 'correct? (ast-child 'RHand n)))))
       
       (Reference
        (lambda (n)
          (att-value 'local-correct? n))))
      
      (ag-rule
       local-correct?
       
       (Declaration
        (lambda (n)
          (and
           (let ((decl (att-value 'lookup-local (ast-parent n) (ast-child 'name n))))
             (or
              (eq? decl n)
              (let ((def (att-value 'lookup-definition decl)))
                (eq? def n))))
           (let ((def (att-value 'lookup-definition n)))
             (or (not def) (equal-types? def n))))))
       
       (Reference
        (lambda (n)
          (let ((decl (att-value 'declaration n)))
            (and
             decl
             (eq? (ast-node-type decl) 'FieldDeclaration))))))))))