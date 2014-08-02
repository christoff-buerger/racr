; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: M. Tasić

#!r6rs

(library
 (cjava-racr name-analysis)
 (export
  specify-name-analysis)
 (import (rnrs) (racr core))
 
 (define do-lookup
   (lambda (n label)
     (define subtree-lookup
       (lambda (n label)
         (ast-find-child*
          (lambda (i n)
            (att-value 'lookup n label))
          (ast-child 'Body n))))
     (let ((local-match (att-value 'local-lookup n (car label))))
       (cond
         ((null? (cdr label))
          local-match)
         (local-match
          (let ((car-label (car label)))
            (cond
              ((eqv? car-label -1)
               (or (att-value 'lookup n (cdr label))
                   (subtree-lookup n label)))
              ((eqv? car-label 0)
               (att-value 'lookup n (cdr label)))
              ((number? car-label)
               (or (att-value 'lookup n (cdr label))
                   (subtree-lookup n (cons (- car-label 1) (cdr label)))))
              (else
               (att-value 'lookup local-match (cdr label))))))
         (else #f)))))
 
 (define specify-name-analysis
   (lambda (specification)
     (with-specification
      specification
      
      (ag-rule
       lookup
       
       (CompilationUnit
        (lambda (n label)
          (do-lookup n label)))
       
       (ClassDeclaration
        (lambda (n label)
          (do-lookup n label)))
       
       (Declaration
        (lambda (n label)
          #f)))
      
      (ag-rule
       local-lookup
       
       (CompilationUnit
        (lambda (n label)
          (ast-find-child
           (lambda (i n)
             (or (number? label) (eq? label '*) (eq? label (ast-child 'name n))))
           (ast-child 'Body n))))
       
       (ClassDeclaration
        (lambda (n label)             
          (ast-find-child
           (lambda (i n)
             (or (number? label) (eq? label '*) (eq? label (ast-child 'name n))))
           (ast-child 'Body n))))
       
       (MethodDeclaration
        (lambda (n label . bound-arg)
          (let ((bound (if (null? bound-arg) (cons 1 '*) (car bound-arg))))
            (or
             (ast-find-child
              (lambda (i n)
                (or (number? label) (eq? label '*) (eq? label (ast-child 'name n))))
              (ast-child 'Parameters n))
             (ast-find-child
              (lambda (i n)
                (and (ast-subtype? n 'Declaration)
                     (or (number? label) (eq? label '*) (eq? label (ast-child 'name n)))))
              (ast-child 'Body n)
              bound))))))
      
      (ag-rule
       lookup-ref
       
       (CompilationUnit
        (lambda (n ref-name)
          (att-value 'lookup n ref-name)))
       
;       (ClassDeclaration ; More RACR like specification for (CompilationUnit Body) and (ClassDeclaration Body) contexts.
;        (lambda (n ref-name)
;          (or
;           (att-value 'lookup n ref-name)
;           (att-value 'lookup-ref (ast-parent (ast-parent n)) ref-name))))
       
       ((CompilationUnit Body)
        (lambda (n ref-name)
          (or
           (att-value 'lookup n ref-name)
           (att-value 'lookup-ref (ast-parent (ast-parent n)) ref-name))))
       
       ((ClassDeclaration Body)
        (lambda (n ref-name)
          (or
           (att-value 'lookup n ref-name)
           (att-value 'lookup-ref (ast-parent (ast-parent n)) ref-name))))
       
       ((MethodDeclaration Body)
        (lambda (n ref-name)
          (or
           (and
            (null? (cdr ref-name))
            (att-value 'local-lookup (ast-parent (ast-parent n)) (car ref-name) (cons 1 (ast-child-index n))))
           (att-value 'lookup-ref (ast-parent (ast-parent n)) ref-name)))))
      
      (ag-rule
       source
       (BindComposer
        (lambda (n)
          (att-value 'lookup n (att-value 'source-name n)))))
      
      (ag-rule
       target
       (BindComposer
        (lambda (n)
          (att-value 'lookup n (att-value 'target-name n)))))))))