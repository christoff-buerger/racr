; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (tinycpp-racr name-analysis)
 (export
  lookup-subtree
  specify-name-analysis)
 (import (rnrs) (racr core))
 
 (define lookup-subtree
   (lambda (n name)
     (if (null? (cdr name))
         (att-value 'lookup-local n (car name))
         (let* ((n (att-value 'lookup-local n (car name)))
                (n (and n (att-value 'lookup-definition n))))
           (and n (ast-subtype? n 'ClassDefinition) (lookup-subtree n (cdr name)))))))
 
 (define lookup-local
   (lambda (n simple-name to-search . bounds)
     (apply
      ast-find-child
      (lambda (i n)
        (eq? (ast-child 'name n) simple-name))
      (ast-child to-search n)
      bounds)))
 
 (define specify-name-analysis
   (lambda (specification)
     (with-specification
      specification
      
      (ag-rule
       lookup-definition
       (Declaration
        (lambda (n)
          (if (att-value 'definition? n)
              n
              (let ((def
                      (lookup-local
                       (ast-parent (ast-parent n))
                       (ast-child 'name n)
                       'Body
                       (cons (+ (ast-child-index n) 1) '*))))
                (and def (att-value 'definition? def) def))))))
      
      (ag-rule
       declaration
       (Reference
        (lambda (n)
          (let* ((name (ast-child 'name n))
                 (decl
                  (if (pair? name)
                      (let ((decl (att-value 'lookup-prefix n (car name))))
                        (and decl (lookup-subtree decl (cdr name))))
                      (att-value 'lookup-simple-reference n name))))
            (and decl (<= (ast-child 'globalindex decl) (ast-child 'globalindex n)) decl)))))
      
      (ag-rule
       lookup-simple-reference
       (MethodDeclaration
        (lambda (n simple-name)
          (or
           (att-value 'lookup-local n simple-name)
           (att-value 'lookup-prefix n simple-name)))))
      
      (ag-rule
       lookup-prefix
       
       (CompilationUnit
        (lambda (n prefix)
          (att-value 'lookup-local n prefix)))
       
       (ClassDeclaration
        (lambda (n prefix)
          (if (eq? (ast-child 'name n) prefix)
              n
              (or
               (att-value 'lookup-local n prefix)
               (att-value 'lookup-prefix (ast-parent n) prefix))))))
      
      (ag-rule
       lookup-local
       (CompilationUnit (lambda (n label) (lookup-local n label 'Body)))
       (ClassDefinition (lambda (n label) (lookup-local n label 'Body)))
       (ClassDeclaration (lambda (n label) #f))
       (MethodDeclaration (lambda (n label) (lookup-local n label 'Parameters))))))))