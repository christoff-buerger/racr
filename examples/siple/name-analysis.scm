; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple name-analysis)
 (export
  specify-name-analysis)
 (import (rnrs) (racr) (siple ast))
 
 (define specify-name-analysis
   (lambda ()
     (with-specification
      siple-specification
      
      (ag-rule
       lookup
       (CompilationUnit
        0
        (lambda (n name)
          (let ((result (list)))
            (ast-for-each-child
             (lambda (i n)
               (if (string=? (ast-child 1 n) name)
                   (set! result (cons n result))))
             (ast-child 1 n))
            result)))
       
       (Block
        1
        (lambda (n name)
          (let* ((statement-list (ast-parent n))
                 (block (ast-parent statement-list))
                 (result (list)))
            (if (att-value 'is-procedure-body block)
                (ast-for-each-child
                 (lambda (i n)
                   (if (string=? (ast-child 1 n) name)
                       (set! result (cons n result))))
                 (ast-child 2 (att-value 'is-procedure-body block))))
            (ast-for-each-child
             (lambda (i n)
               (if (and (ast-subtype? n 'Declaration) (string=? (ast-child 1 n) name))
                   (set! result (cons n result))))
             statement-list
             (cons 1 (ast-child-index n)))
            (if (> (length result) 0)
                result
                (att-value 'lookup block name))))))
      
      (ag-rule
       main-procedure
       (CompilationUnit
        0
        (lambda (n)
          (let ((result (att-value 'lookup n "main")))
            (if (and (= (length result) 1) (ast-subtype? (car result) 'ProcedureDeclaration))
                (car result)
                #f)))))
      
      (ag-rule
       declaration
       (Reference
        0
        (lambda (n)
          (let ((result (att-value 'lookup n (ast-child 1 n))))
            (if (= (length result) 1)
                (car result)
                #f)))))))))