; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (tinycpp-racr name-analysis)
 (export
  specify-name-analysis)
 (import (rnrs) (racr core))
 
 (define lookup-subtree
   (lambda (n label)
     (cond
       ((null? (cdr label))
        (att-value 'lookup-local n (car label)))
       (else
        (let* ((reversed-label (reverse label))
               (still-to-lookup (car reversed-label))
               (new-n (att-value 'lookup-subtree n (reverse (cdr reversed-label)))))
          (and
           new-n
           (ast-subtype? new-n 'ClassDeclaration)
           (att-value 'lookup-local new-n still-to-lookup)))))))
 
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
       lookup-subtree
       (CompilationUnit lookup-subtree)
       (ClassDefinition lookup-subtree)
       (ClassDeclaration (lambda (n label) #f)))
      
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
      
      (ag-rule
       lookup-reference
       
       ((CompilationUnit Body)
        (lambda (n label)
          (let ((decl (att-value 'lookup-subtree (ast-parent (ast-parent n)) label)))
            (and
             decl
             (<= (att-value 'global-index decl) (ast-child-index n))
             decl))))
       
       ((ClassDefinition Body)
        (lambda (n label)
          (let* ((class (ast-parent (ast-parent n)))
                 (local-decl (att-value 'lookup-local class (car label))))
            (if (and local-decl (or (not (ast-subtype? local-decl 'Constructor)) (null? (cdr label))))
                (att-value 'lookup-subtree class label)
                (att-value 'lookup-reference class label)))))
       
       ((MethodDeclaration Body)
        (lambda (n label)
          (let ((method (ast-parent (ast-parent n))))
            (or
             (and
              (null? (cdr label))
              (att-value 'lookup-local method (car label)))
             (att-value 'lookup-reference method label))))))
      
      (ag-rule
       declaration
       (Reference
        (lambda (n)
          (att-value 'lookup-reference n (ast-child 'name n)))))))))