; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (tinycpp-racr support-api)
 (export
  specify-support-api
  equal-types?)
 (import (rnrs) (racr core))
 
 (define equal-types?
   (lambda (n1 n2)
     (cond
       ((ast-subtype? n1 'ClassDeclaration)
        (ast-subtype? n2 'ClassDeclaration))
       ((ast-subtype? n1 'FieldDeclaration)
        (ast-subtype? n2 'FieldDeclaration))
       (else
        (and
         (ast-subtype? n2 'MethodDeclaration)
         (let ((paras1 (ast-children (ast-child 'Parameters n1)))
               (paras2 (ast-children (ast-child 'Parameters n2))))
           (and
            (= (length paras1) (length paras2))
            (for-all
                (lambda (n1 n2)
                  (equal-types? n1 n2))
              paras1
              paras2))))))))
 
 (define specify-support-api
   (lambda (specification)
     (with-specification
      specification
      
      (ag-rule
       definition?
       (Declaration (lambda (n) #f))
       (FieldDeclaration (lambda (n) #t))
       (MethodDeclaration (lambda (n) #t))
       (ClassDefinition (lambda (n) #t)))))))