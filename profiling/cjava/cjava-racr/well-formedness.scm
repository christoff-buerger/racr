; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: M. Tasić

#!r6rs

(library
 (cjava-racr well-formedness)
 (export
  specify-well-formedness)
 (import (rnrs) (racr core) (cjava-racr name-analysis))
 
 (define specify-well-formedness
   (lambda (siple-specification)
     (with-specification
      siple-specification
      
      (ag-rule
       correct?
       
       (CompilationUnit
        (lambda (n)
          (not
           (ast-find-child
            (lambda (i n)
              (not (att-value 'correct? n)))
            (ast-child 'Body n)))))
       
       (Statement
        (lambda (n)
          (att-value 'local-correct? n)))
       
       (ClassDeclaration
        (lambda (n)
          (and
           (att-value 'local-correct? n)
           (not
            (ast-find-child
             (lambda (i n)
               (not (att-value 'correct? n)))
             (ast-child 'Body n))))))
       
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
           (att-value 'correct? (ast-child 'RHand n))))))
      
      (ag-rule
       local-correct?
       
       (Statement
        (lambda (n)
          #t))
       
       (Declaration
        (lambda (n)
          (eq? (att-value 'local-lookup (ast-parent (ast-parent n)) (ast-child 'name n)) n)))
       
       (Reference
        (lambda (n)
          (let ((decl (att-value 'lookup-ref n (ast-child 'name n))))
            (and
             decl
             (eq? (ast-node-type decl) 'FieldDeclaration))))))))))