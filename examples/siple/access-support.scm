; This program and the accompanying materials are made available under the
; terms of the MIT license (X11 license) which accompanies this distribution.

; Author: C. Bürger

#!r6rs

(library
 (siple access-support)
 (export
  specify-access-support)
 (import (rnrs) (racr core))
 
 (define specify-access-support
   (lambda (siple-specification)
     (with-specification
      siple-specification
      
      (ag-rule
       dewey-address
       (CompilationUnit
        (lambda (n)
          (list 1)))
       (Statement
        (lambda (n)
          (append
           (att-value 'dewey-address (ast-parent n))
           (if (ast-list-node? (ast-parent n))
               (list (ast-child-index (ast-parent n)))
               (list))
           (list (ast-child-index n))))))
      
      (ag-rule
       is-procedure-body
       ((Block Statement*)
        (lambda (n)
          #f))
       ((ProcedureDeclaration Body)
        (lambda (n)
          (ast-parent n))))
      
      (ag-rule
       procedure-in-context
       (CompilationUnit
        (lambda (n)
          #f))
       ((ProcedureDeclaration Body)
        (lambda (n)
          (ast-parent n))))
      
      (ag-rule
       procedure-return-in-context
       (CompilationUnit
        (lambda (n)
          #f))
       (ProcedureReturn
        (lambda (n)
          n)))
      
      (ag-rule
       as-boolean
       (Constant
        (lambda (n)
          (let ((lexem (ast-child 'lexem n)))
            (cond
              ((string=? lexem "true") #t)
              ((string=? lexem "false") #f)
              (else 'siple:nil))))))
      
      (ag-rule
       as-number
       (Constant
        (lambda (n)
          (let ((number (string->number (ast-child 'lexem n))))
            (if number number 'siple:nil)))))
      
      (ag-rule
       as-integer
       (Constant
        (lambda (n)
          (if (not (find
                    (lambda (c) (char=? c #\.))
                    (string->list (ast-child 'lexem n))))
              (att-value 'as-number n)
              'siple:nil))))
      
      (ag-rule
       as-real
       (Constant
        (lambda (n)
          (if (find
               (lambda (c) (char=? c #\.))
               (string->list (ast-child 'lexem n)))
              (att-value 'as-number n)
              'siple:nil))))))))