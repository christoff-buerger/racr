; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (tinycpp-racr name-analysis)
 (export
  specify-name-analysis)
 (import (rnrs) (racr core))
 
 (define lookup-local
   (lambda (n label to-search . bounds)
     (apply
      ast-find-child
      (lambda (i n)
        (eq? (ast-child 'name n) label))
      (ast-child to-search n)
      bounds)))
 
 (define specify-name-analysis
   (lambda (specification)
     (with-specification
      specification
      
      (ag-rule
       global-index
       ((CompilationUnit Body)
        (lambda (n)
          (ast-child-index n))))
      
      (ag-rule
       declaration
       (Reference
        (lambda (n)
          (let* ((name (ast-child 'name n))
                 (decl
                  (if (pair? name)
                      (let ((decl (att-value 'lookup-prefix n (car name))))
                        (and
                         decl
                         (att-value
                          'lookup-global
                          (att-value 'compilation-unit decl)
                          (fold-left
                           (lambda (result prefix)
                             (cons result prefix))
                           (att-value 'full-qualified-name decl)
                           (cdr name)))))
                      (att-value 'lookup-simple-reference n name))))
            (and decl (<= (att-value 'global-index decl) (att-value 'global-index n)) decl)))))
      
      (ag-rule
       lookup-simple-reference
       (MethodDeclaration
        (lambda (n name)
          (or
           (att-value 'lookup-local n name)
           (att-value 'lookup-prefix n name)))))
      
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
       lookup-global
       (CompilationUnit
        (lambda (n label)
          (if (pair? label)
              (let* ((new-decl (att-value 'lookup-global n (car label)))
                     (new-def (and new-decl (att-value 'lookup-definition new-decl))))
                (and
                 new-def
                 (ast-subtype? new-def 'ClassDefinition)
                 (att-value 'lookup-local new-def (cdr label))))
              (att-value 'lookup-local n label)))))
      
      (ag-rule
       lookup-local
       (CompilationUnit (lambda (n label) (lookup-local n label 'Body)))
       (ClassDefinition (lambda (n label) (lookup-local n label 'Body)))
       (ClassDeclaration (lambda (n label) #f))
       (MethodDeclaration (lambda (n label) (lookup-local n label 'Parameters))))
      
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
      ))))